package workshop.jdbc;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import com.ibm.ims.dli.DLIException;
import com.ibm.ims.jdbc.IMSDataSource;

public class JdbcApiDemo {
	public static void main(String[] args) throws DLIException, SQLException {
		IMSDataSource ds = new IMSDataSource();
		ds.setDatastoreServer("your.host.name");
		ds.setPortNumber(5555);
		ds.setDriverType(4);
		ds.setDatastoreName("IMS1");
		ds.setDatabaseName("BMP255");
		
		Connection conn = ds.getConnection();
		
		System.out.println("\n>>>>>>>>>>>>>>>>>>> displayAllHospitalRecords >>>>>>>>>>>>>>>>>\n");
		
		displayAllHospitalRecords(conn);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>> getRecord >>>>>>>>>>>>>>>>>>\n");
		
		getRecord(conn);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>> deleteRecord >>>>>>>>>>>>>>>>>>\n");
		
		deleteRecord(conn);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>> insertRecord >>>>>>>>>>>>>>>>>>\n");
		
		insertRecord(conn);
		
		System.out.println("\n>>>>>>>>>>>>>>>>>> replaceRecord >>>>>>>>>>>>>>>>>>\n");
		
		replaceRecord(conn);
		
		conn.rollback();
		conn.close();
	}
	
	public static void displayAllHospitalRecords(Connection conn) throws SQLException {
		String selectStatement = "SELECT HOSPNAME, HOSPCODE FROM PCB01.HOSPITAL";
		
		Statement st = conn.createStatement();
		ResultSet rs = st.executeQuery(selectStatement);
		
		String hospitalName;
		String hospitalCode;
		while(rs.next()) {
			hospitalName = rs.getString("HOSPNAME");
			hospitalCode = rs.getString(2);
			System.out.println("HOSPITAL NAME: " + hospitalName + ", HOSPITAL CODE: " + hospitalCode);
		}
		rs.close();
		st.close();
	}
	
	public static void deleteRecord(Connection conn) throws SQLException {
		String deleteStatement = "DELETE FROM PCB01.WARD WHERE HOSPITAL_HOSPCODE = 'R1210010000A' and WARDNO = '0200'";
		
		Statement st = conn.createStatement();
		int rowsChanged = st.executeUpdate(deleteStatement);
		
		System.out.println("Deleted " + rowsChanged + " row(s)");
	}
	
	public static void insertRecord(Connection conn) throws SQLException {
		String insertStatement = "INSERT INTO PCB01.WARD (HOSPITAL_HOSPCODE, WARDNO, WARDNAME) VALUES ('R1210010000A', '0200', 'NEW WARD')";
		String selectStatement = "SELECT WARDNO, WARDNAME FROM PCB01.WARD WHERE HOSPITAL_HOSPCODE ='R1210010000A' AND WARDNO = '0200'";
		
		Statement st = conn.createStatement();
		int rowsInserted = st.executeUpdate(insertStatement);
		System.out.println("Inserted " + rowsInserted + " rows.");
		
		ResultSet rs = st.executeQuery(selectStatement);
		while (rs.next()) {
			String wardName = rs.getString("WARDNAME");
			String wardNumber = rs.getString("WARDNO");
			System.out.println("WARD NUMBER: " + wardNumber + "\nWARD NAME: " + wardName);
		}
		
		String sqlToDLI = conn.nativeSQL(insertStatement);
		System.out.println(sqlToDLI);
		
		conn.rollback();
	}
	
	public static void replaceRecord(Connection conn) throws SQLException {
		String selectStatement = "SELECT HOSPNAME FROM PCB01.HOSPITAL WHERE HOSPCODE='R1210010000A'";
		String updateStatement = "UPDATE PCB01.HOSPITAL SET HOSPNAME = 'ALEXANDRIA' WHERE HOSPCODE = 'R1210010000A'";
		
		Statement st = conn.createStatement();
		ResultSet rs = st.executeQuery(selectStatement);
		while (rs.next()) {
			String hospitalName = rs.getString("HOSPNAME");
			System.out.println("Hospital Name: " + hospitalName);
		}
		rs.close();
		
		int rowsUpdated = st.executeUpdate(updateStatement);
		System.out.println("Updated " + rowsUpdated + " rows.");
		
		rs = st.executeQuery(selectStatement);

		while (rs.next()) {
			String hospitalName = rs.getString("HOSPNAME");
			System.out.println("Updated Hospital Name: " + hospitalName);
		}
		rs.close();
		st.close();
		
		conn.rollback();
	}
	
	public static void getRecord(Connection conn) throws SQLException {
		String selectStatement = "SELECT HOSPNAME, HOSPCODE FROM PCB01.HOSPITAL WHERE HOSPCODE = ? ";
		String hospitalCode = "R1210010000A";
		
		// Support both PreparedStatements and Statements
		PreparedStatement pst = conn.prepareStatement(selectStatement);
		pst.setString(1, hospitalCode);
		
		ResultSet rs = pst.executeQuery();
		String hospitalName;
		while(rs.next()) {
			hospitalName = rs.getString("HOSPNAME");
			hospitalCode = rs.getString(2);
			System.out.println("HOSPITAL NAME: " + hospitalName + ", HOSPITAL CODE: " + hospitalCode);
		}
		rs.close();
		pst.close();
		
		conn.rollback();
	}
}
