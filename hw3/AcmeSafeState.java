import java.util.concurrent.atomic.AtomicLongArray;
class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
      long[] ret_value = new long[value.length()];
      for (int i = 0; i < ret_value.length; i++) {
        ret_value[i] = value.get(i);
      }
      return ret_value;
    }

    public void swap(int i, int j) {
    	value.getAndDecrement(i);
    	value.getAndIncrement(j);
    }
}
