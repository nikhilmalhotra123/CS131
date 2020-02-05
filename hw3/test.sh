echo "Type, State, #Threads, Swap Avg Real (ns), Swap Avg CPU (ns), Mismatch" > results.csv
echo -n "Synchronized, 5, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 1 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 5, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 8 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 5, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 20 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 5, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 40 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 50, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 1 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 50, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 8 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 50, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 20 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 50, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 40 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 100, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 1 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 100, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 8 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 100, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 20 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Synchronized, 100, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Synchronized 40 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 5, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 1 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 5, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 8 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 5, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 20 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 5, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 40 100000000 5 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 50, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 1 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 50, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 8 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 50, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 20 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 50, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 40 100000000 50 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 100, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 1 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 100, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 8 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 100, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 20 100000000 100 | awk 'NR==2' >> results.csv
echo -n "Unsynchronized, 100, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest Unsynchronized 40 100000000 100 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 5, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 1 100000000 5 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 5, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 8 100000000 5 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 5, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 20 100000000 5 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 5, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 40 100000000 5 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 50, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 1 100000000 50 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 50, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 8 100000000 50 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 50, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 20 100000000 50 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 50, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 40 100000000 50 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 100, 1, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 1 100000000 100 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 100, 8, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 8 100000000 100 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 100, 20, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 20 100000000 100 | awk 'NR==2' >> results.csv
echo -n "AcmeSafe, 100, 40, " >> results.csv
time timeout 3600 java UnsafeMemoryTest AcmeSafe 40 100000000 100 | awk 'NR==2' >> results.csv
