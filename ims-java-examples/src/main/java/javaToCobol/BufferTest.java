package javaToCobol;

import java.nio.ByteBuffer;

public class BufferTest {
	static {
		// This attempts to find an load a DLL from 
		// your LIBPATH called libWrapper.so (name is a user choice).
		// 31-bit applications will load lib<name>.so
		// 64-bit applications will load lib<name>_64.so
		System.loadLibrary("Wrapper");
	}

	// Defines the method signature for the the OO COBOL wrapper implementation
	static native int callCob1(java.nio.ByteBuffer BB);

	public static void main(String argv[]) throws Exception {
		System.out.println("JAVA:>> main(String argv) invoked. Building buffer and calling COBOL");

		/*
		 * ByteBuffer.allocateDirect allocates native memory off of the JVM heap
		 * which allows sharing the memory via pointer between COBOL and Java
		 * if you instead use a byte[] or ByteBuffer.allocate this will copy data
		 * between the native layers rather than passing pointers.
		 * 
		 * You will need to be concious if you are running 31-bit or 64-bit JVMs
		 * because allocateDirect will create Native memory in the JVMs bit mode.
		 * We have written native layers to malloc memory in C++ and wrap that 
		 * as a direct bytebuffer as well but this requires you manage the storage
		 * more closely.
		 */
		ByteBuffer bb = ByteBuffer.allocateDirect(40);
		
		/*
		 * Set some data into the ByteBuffer, ideally you would create
		 * a Java class and a COBOL structure to better control the mapping
		 * of this data area.
		 */
		bb.put("KEVIN ".getBytes("Cp1047"));
		bb.put("Engineer".getBytes("Cp1047"));
		bb.put("02/12/17".getBytes("Cp1047"));

		// Calls into the COBOL native implementation in wrapper.cbl and then into cob1.cbl
		int i = BufferTest.callCob1(bb);

		System.out.println("JAVA:>> ADDRESS OF BYTEBUFFER SENT FROM COBOL: " + i);
		bb.position(0);
		byte[] nameBytes = new byte[6];
		bb.get(nameBytes, 0, 6);

		System.out.println("JAVA:>> Java invoked COBOL to change NAME to: " + new String(nameBytes, "Cp1047"));
	}
}