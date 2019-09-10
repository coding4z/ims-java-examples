package cobolToJava;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class CobolToJava {
	private static final Charset EBCDIC = Charset.forName("Cp1047");

	public static void runTest(ByteBuffer customerInfoByteBuffer) {
		byte[] customerNameBytes = new byte[6];
		byte[] customerJobBytes = new byte[8];
		byte[] customerDateBytes = new byte[9];
		
		/*
		 * Starting at the beginning of the ByteBuffer
		 * copy each field into a byte array.
		 * 
		 * If the ByteBuffer contained native types
		 * like int, float, long, short, etc 
		 * you don't need to copy to a byte array first
		 * instead use ByteBuffer.get<Type>() to get the 
		 * value directly.
		 */
		customerInfoByteBuffer.position(0);
		customerInfoByteBuffer.get(customerNameBytes);
		customerInfoByteBuffer.get(customerJobBytes);
		customerInfoByteBuffer.get(customerDateBytes);
		
		/*
		 * Convert the byte arrays for each field to a string
		 */
		String customerName = new String(customerNameBytes, EBCDIC);
		String customerJob = new String(customerJobBytes, EBCDIC);
		String customerDate = new String(customerDateBytes, EBCDIC);
		
		/*
		 * Display the values in Java
		 */
		System.out.println("Java >> customerName received from COBOL: " + customerName);
		System.out.println("Java >> customerJob  received from COBOL: " + customerJob);
		System.out.println("Java >> customerDate received from COBOL: " + customerDate);
		
		/*
		 * Update the name to DEEPAK to show that COBOL will have visibility
		 * to the name change and prove that the storage is shared.
		 */
		customerInfoByteBuffer.position(0);
		customerInfoByteBuffer.put("DEEPAK".getBytes(Charset.forName("Cp1047")));
	}
}
