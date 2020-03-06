import asyncio
import argparse
import config
import sys
import time
import aiohttp
import apikey
import json
import logging


clients = {
    # client_name : [server: abc, time_diff: 123, coord: 131+32, update_time: 123]
}

def splitlocation(location):
    count = 0
    for i in range(0, len(location)):
        if location[i] == '+' or location[i]=='-':
            count += 1
            if count == 2:
                lat = location[:i]
                long = location[i:]
    return [lat, long]

def isIAMAT(message):
    if len(message) == 4 and message[0] == "IAMAT":
        return True
    return False

async def handleIAMAT(clientid, location, timerec, timesent, writer):
    # TODO: validate inputs
    timesent = float(timesent)
    timediff = timerec - timesent
    if timediff > 0:
        timediff = "+" + str(timediff)
    else:
        timediff = str(timediff)

    if clientid in clients and clients[clientid]['update_time'] > timesent:
        return

    clients[clientid] = {
        'server': name,
        'timediff': timediff,
        'coord': location,
        'update_time': timesent
    }

    sendback_message = 'AT {} {} {} {} {}\n'.format(name, timediff, clientid, location, timesent)
    logging.info("Sending: " + sendback_message)
    writer.write(sendback_message.encode())
    await flood(clientid)

def isWHATSAT(message):
    if len(message) == 4 and message[0] == "WHATSAT":
        return True
    return False

async def handleWHATSAT(clientid, radius, infosize, writer):
    if radius > 50 or infosize > 20 or clientid not in clients:
        sendback_message = '? WHATSAT {} {} {}\n'.format(clientid, radius, infosize)
    else:
        lat, long = splitlocation(clients[clientid]['coord'])

        url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(apikey.APIKEY, lat + "," + long, float(radius))

        # async with aiohttp.ClientSession() as session:
        #     async with session.get(url) as resp:
        #         response = await resp.json()
        #         response['results'] = response['results'][:infosize]
        #         sendback_message = 'AT {} {} {} {} {}\n{}\n\n'.format(clients[clientid]['server'], clients[clientid]['timediff'], clientid, clients[clientid]['coord'], clients[clientid]['update_time'], json.dumps(response, indent=4))
    logging.info("Sending: " + sendback_message)
    writer.write(sendback_message.encode())

def isAT(message):
    if len(message) == 6 and message[0] == 'AT':
        return True
    return False

async def handleAT(server, timediff, clientid, location, timesent, writer):
    if clientid in clients and float(clients[clientid]['update_time']) >= float(timesent):
        return # Already updated server

    logging.info(server)
    clients[clientid] = {
        'server': server,
        'timediff': timediff,
        'coord': location,
        'update_time': timesent
    }

    await flood(clientid)

async def flood(clientid):
    params = clients[clientid]
    send_message = 'AT {} {} {} {} {}\n'.format(params['server'], params['timediff'], clientid, params['coord'], params['update_time'])
    logging.info("Begin propogating message: " + send_message)
    for server in config.NEIGHBORS[name]:
        port_num = config.SERVER_PORTS_LOCAL[server]
        try:
            reader, writer = await asyncio.open_connection('127.0.0.1', port_num, loop=loop)
            logging.info("Sending: " + send_message)
            writer.write(send_message.encode())
        except:
            logging.error("Failed to connect to " + server + " at port " + str(port_num))
    logging.info("End propogating message")


async def handle_echo(reader, writer):
    try:
        addr = writer.get_extra_info('peername')
        logging.info("Opened connection with " + str(addr))
        try:
            data = await reader.read(int(1e6))
            message = data.decode()
            logging.info("Recieved: " + str(message))
            timerec= time.time()

            split_message = [i for i in message.split(" ") if i]

            if isIAMAT(split_message):
                await handleIAMAT(split_message[1], split_message[2], timerec, split_message[3], writer)

            elif isWHATSAT(split_message):
                await handleWHATSAT(split_message[1], int(split_message[2]), int(split_message[3]), writer)

            elif isAT(split_message):
                await handleAT(split_message[1], split_message[2], split_message[3], split_message[4], split_message[5], writer)

            else:
                sendback_message = '? ' + message + '\n'
                writer.write(sendback_message.encode())

        except asyncio.streams.IncompleteReadError as e:
            logging.error("Failed to read input")
    except (ConnectionResetError, ConnectionAbortedError) as e:
        logging.error("Connection failed")
        pass

    logging.info("Closed connection with " + str(addr))
    await writer.drain()
    writer.close()

def main():
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    if args.server_name not in config.SERVER_NAMES:
        print("Invalid server name. Server name not found in ", config.SERVER_NAMES)
        sys.exit(1)

    global name
    name = args.server_name
    port = config.SERVER_PORTS_LOCAL[name] # TODO: Change back from local

    logFile = str(name) + ".log"
    logging.basicConfig(filename=logFile, level=logging.INFO
    )

    # https://docs.python.org/3/library/asyncio-protocol.html
    global loop
    loop = asyncio.get_event_loop()
    coro = asyncio.start_server(handle_echo, '127.0.0.1', port, loop=loop)
    server = loop.run_until_complete(coro)

    print('Serving on {}'.format(server.sockets[0].getsockname()))

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    # Close the server
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
    logging.info("Server closed")


if __name__ == '__main__':
    main()
