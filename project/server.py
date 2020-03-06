import asyncio
import argparse
import config
import sys
import time


clients = {
    # client_name : [server: abc, time_diff: 123, coord: 131+32, update_time: 123]
}

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
        return None

    clients[clientid] = {
        'server': name,
        'time_diff': timediff,
        'coord': location,
        'update_time': timesent
    }

    sendback_message = 'AT {} {} {} {} {}\n'.format(name, timediff, clientid, location, timesent)
    writer.write(sendback_message.encode())
    await writer.drain()

async def handle_echo(reader, writer):
    data = await reader.read(int(1e6)) # TODO: fix message length
    message = data.decode()
    addr = writer.get_extra_info('peername')
    timerec= time.time()
    print("{} received {} from {}".format(name, message, addr))

    split_message = [i for i in message.split(" ") if i]

    if isIAMAT(split_message):
        await handleIAMAT(split_message[1], split_message[2], timerec, split_message[3], writer)

    print("close the client socket")
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

    print("Hello, welcome to server {}".format(name))
    # https://docs.python.org/3/library/asyncio-protocol.html
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


if __name__ == '__main__':
    main()
