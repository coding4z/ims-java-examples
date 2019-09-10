package workshop.dli;

import com.ibm.ims.dli.DLIException;
import com.ibm.ims.dli.IMSConnectionSpec;
import com.ibm.ims.dli.IMSConnectionSpecFactory;
import com.ibm.ims.dli.PCB;
import com.ibm.ims.dli.PSB;
import com.ibm.ims.dli.PSBFactory;
import com.ibm.ims.dli.Path;
import com.ibm.ims.dli.PathSet;
import com.ibm.ims.dli.SSAList;

public class DliApiDemo {
	public static void main(String[] args) throws Exception {
		IMSConnectionSpec connSpec = IMSConnectionSpecFactory.createIMSConnectionSpec();
		connSpec.setDatastoreServer("your.host.name");
		connSpec.setPortNumber(5555);
		connSpec.setDriverType(4);
		connSpec.setDatabaseName("BMP255");
		connSpec.setDatastoreName("IMS1");
		connSpec.setUser("SAFUSER");
		connSpec.setPassword("SAFPASS");
		
		System.out.println("\n>>>>>>>>>>>>>>>>>>> displayAllHospitalRecords >>>>>>>>>>>>>>>>>\n");
		
		displayAllHospitalRecords(connSpec);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>>> getRecord >>>>>>>>>>>>>>>>>\n");
		
		getRecord(connSpec);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>>> insertRecord >>>>>>>>>>>>>>>>>\n");
		
		insertRecord(connSpec);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>>> replaceRecord >>>>>>>>>>>>>>>>>\n");
		
		replaceRecord(connSpec);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>> displayAllHospitalRecordsBatchRetrieve >>>>>>>>>>>>>>>>>>\n");
		
		displayAllHospitalRecordsBatchRetrieve(connSpec);
	}
	
	public static void insertRecord(IMSConnectionSpec connSpec) throws DLIException {
		PSB psb = PSBFactory.createPSB(connSpec);
		PCB pcb = psb.getPCB("PCB01");
		
		SSAList ssaList = pcb.getSSAList("HOSPITAL", "WARD");
		ssaList.addInitialQualification("HOSPITAL", "HOSPCODE", SSAList.EQUALS, "R1210010000A");

		Path path = ssaList.getPathForInsert("WARD");
		path.setString("WARDNO", "0200");
		path.setString("WARDNAME", "NEW WARD");

		pcb.insert(path, ssaList);
		
		ssaList.addInitialQualification("WARD", "WARDNO", SSAList.EQUALS, "0200");
		System.out.println(ssaList.toString() + "\n");
	
		path = ssaList.getPathForRetrieveReplace();
		pcb.getUnique(path, ssaList, false);
		
		String wardNumber = path.getString("WARDNO");
		String wardName = path.getString("WARDNAME");
		System.out.println("WARD NUMBER: " + wardNumber + "\nWARD NAME: " + wardName);
		
		psb.rollback();
		psb.close();
	}
	
	public static void replaceRecord(IMSConnectionSpec connSpec) throws DLIException {
		PSB psb = PSBFactory.createPSB(connSpec);
		PCB pcb = psb.getPCB("PCB01");
		
		SSAList ssaList = pcb.getSSAList("HOSPITAL");
		ssaList.addInitialQualification("HOSPITAL", "HOSPCODE", SSAList.EQUALS, "R1210010000A");
		
		Path path = ssaList.getPathForRetrieveReplace();
		pcb.getUnique(path, ssaList, true);
		String hospitalName = path.getString("HOSPNAME");
		
		System.out.println("HOSPITAL NAME: " + hospitalName);
	
		path.setString("HOSPNAME", "NEW HOSPITAL");
		pcb.replace(path);
		
		pcb.getUnique(path, ssaList, false);
		hospitalName = path.getString("HOSPNAME");
		
		System.out.println("Updated Hospital name: " + hospitalName);
		
		psb.commit();
		psb.close();
	}
	
	public static void getRecord(IMSConnectionSpec connSpec) throws DLIException {
		PSB psb = PSBFactory.createPSB(connSpec);
		PCB pcb = psb.getPCB("PCB01");
		SSAList ssaList = pcb.getSSAList("HOSPITAL");
		
		ssaList.addInitialQualification("HOSPITAL", "HOSPCODE", SSAList.EQUALS, "R1210010000A");
		Path path = ssaList.getPathForRetrieveReplace();
		
		pcb.getUnique(path, ssaList, false);
		
		String hospitalName = path.getString("HOSPNAME");

		System.out.println("HOSPITAL NAME: " + hospitalName);
		
		psb.commit();
		psb.close();
	}

	public static void displayAllHospitalRecords(IMSConnectionSpec connSpec) throws Exception {
		PSB psb = PSBFactory.createPSB(connSpec);
		PCB pcb = psb.getPCB("PCB01");
		SSAList ssaList = pcb.getSSAList("HOSPITAL");
		
		Path path = ssaList.getPathForRetrieveReplace();
		
		String hospitalName;
		String hospitalCode;
		if(pcb.getUnique(path, ssaList, false)) {
			hospitalName = path.getString("HOSPNAME");
			hospitalCode = path.getString("HOSPCODE");
			System.out.println("HOSPITAL NAME: " + hospitalName + ", HOSPITAL CODE: " + hospitalCode);
			
			while(pcb.getNext(path, ssaList, false)) {
				hospitalName = path.getString("HOSPNAME");
				hospitalCode = path.getString("HOSPCODE");
				System.out.println("HOSPITAL NAME: " + hospitalName + ", HOSPITAL CODE: " + hospitalCode);
			}
		}
		
		psb.commit();
		psb.deallocate();
		psb.close();
	}
	
	public static void displayAllHospitalRecordsBatchRetrieve(IMSConnectionSpec connSpec) throws Exception {
		PSB psb = PSBFactory.createPSB(connSpec);
		PCB pcb = psb.getPCB("PCB01");
		SSAList ssaList = pcb.getSSAList("HOSPITAL");
		
		PathSet pathSet = pcb.batchRetrieve(ssaList);
		Path path;
		String hospitalName;
		String hospitalCode;
		while(pathSet.hasNext()) {
			path = pathSet.next();
			hospitalName = path.getString("HOSPNAME");
			hospitalCode = path.getString("HOSPCODE");
			System.out.println("HOSPITAL NAME: " + hospitalName + ", HOSPITAL CODE: " + hospitalCode);
		}
		
		psb.commit();
		psb.deallocate();
		psb.close();
	}
}
