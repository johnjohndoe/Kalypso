package de.tuhh.kalypso.data;
/**
* I_Filter.java
*
* @author Christoph Küpferle
*/


import javax.swing.JFileChooser;
import java.io.File;
import java.io.FileWriter;
import java.util.Vector;
import java.util.Hashtable;
import java.io.IOException;
import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.data.strand.Strand;
import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.util.ErrorHandler.KalypsoFilterException;
import javax.swing.JOptionPane;
import de.tuhh.kalypso.data.timeseries.Timeseries;
import java.util.HashSet;

import de.tuhh.wb.javagis.tools.xml.ServiceTools;

public class I_FilterImpl implements I_Filter
{
    // returns xml-format
    
    public static void main(String args[]) throws Exception
    {
	try
		{
			switch(args.length)
			{
				case 1:
					{
					    I_Filter filter=new I_FilterImpl();
					    String xmlFile=args[0];
					    File files[]=getInputFiles("Import NA-ASCII Files");
						filter.importASCIIFilesToDb(files,xmlFile);
					}
					break;
				case 3:
					{
						I_Filter filter=new I_FilterImpl();
						String xmlSource=args[0];
						String exportFileName=args[1];
						Integer rootNodeNumber=new Integer(args[2]);
						filter.exportASCIIFiles( xmlSource,exportFileName,rootNodeNumber,new HashSet(),new HashSet(),new HashSet());
					}
					break;
				case 4:
					{
						I_Filter filter=new I_FilterImpl();
						String resSource = args[0];
						String resTarget = args[1];
						String pattern = args[2];
						String seperator = args[3];
						filter.importResulsts( new File( resSource ), new File( resTarget ), pattern, seperator );
					}
					break;
				default:
					System.out.println("usage:.....");
			}
		}
		catch( Exception e )
		    {
			e.printStackTrace();
			closeFilter();
		}
    }
    
    public I_FilterImpl() throws Exception
    {
		LogFile.init( "kalypso",".log" );
    }
	/** This method imports BCENA ascii files and writes the data into an XML
	 * file. This file can then be the import file for the database (Jboss).
	 * @param asciiSources An array containing the ascii files to be imported.
	 * @param xmlFileName The path and filename of the destination file (+++.xml)
	 */
    public void importASCIIFilesToDb( File asciiSources[],String xmlFileName) throws Exception
    {
		File netFile=null;
		File rbFile=null;
		File channelFile=null;
		File rhbFile=null;
		File rhtFile=null;
		
		String fileName;
		for(int i=0; i<asciiSources.length; i++)
	    {
			fileName=asciiSources[i].getName();
			
			// net-file
			if(fileName.matches(".+\\.(n|N)(t|T)(z|Z)$"))
				netFile=asciiSources[i];
			
			// riverbasin-file
			if(fileName.matches(".+\\.(g|G)(e|E)(b|B)$"))
				rbFile=asciiSources[i];
			
			// channelFile
			if(fileName.matches(".+\\.(g|G)(e|E)(r|R)$"))
				channelFile=asciiSources[i];
			
			// rhbFile
			if(fileName.matches(".+\\.(r|R)(h|H)(b|B)$"))
				rhbFile=asciiSources[i];
			
			// rhtFile
			if(fileName.matches(".+\\.(r|R)(h|H)(t|T)$"))
				rhtFile=asciiSources[i];
		}
		try
		{
			State wcState = State.create( netFile, rbFile, channelFile, rhbFile, rhtFile );
			wcState.writeStateToXMLFile( new File( xmlFileName ));
			System.out.println( "export to XML-file was successful: END OF PROCESS" );
			LogFile.log( "The " + xmlFileName + " has been successfully written" );
			wcState.stateStatsToLogFile();
		}
		catch( Exception e)
		{
			System.out.println("Sorry, import was not successfull, because: " + e.getMessage());
			LogFile.log("Sorry, import was not successfull, because: " + e.getMessage());
			throw e;
		}
		finally
		{
			closeFilter();
		}
	}
	
	/** This method exports an xml-file to the BCENA ascii-files. It implementes the
	 * routing algorythem which determines the order of the strands in the BCENA net-file
	 * (+++.ntz).
	 * @param xmlSource The path and the name of the xml source file.
	 * @param exportFileName The path and the name of the ascii-files created from the xml File.
	 * @param rootNodeNumber The number of the root node of the system. To select the whole system choose the last node in the system.
	 */
	
    public void exportASCIIFiles( String xmlSource, String exportFileName, Integer rootNodeNumber, HashSet allClimateFiles,HashSet allShortTermFiles,HashSet allLongTermFiles ) throws Exception
    {
	// HYDROTOP-FILE...
	try
	    {
		File xmlFile=new File(xmlSource);
 		File tempFile=File.createTempFile( "kalypso_transform_tmp_", ".xml" );
		File hydrotopFile=new File(exportFileName+".hyd");
		ServiceTools.xslTransform(xmlFile ,new File("xsl/xml_2_hydrotop_part1.xsl"),tempFile);
		ServiceTools.xslTransform(tempFile,new File("xsl/xml_2_hydrotop_part2.xsl"),hydrotopFile);
		tempFile.delete();
		/*

		//xmlFile:	xmlSource
		//hydrotopfile: exportFileName+.hyd
		File xmlFile=new File(xmlSource);
		File xslFile=new File("xsl/xml2hydrotop.xsl");
		System.out.println("starting transformation");
		Runtime.getRuntime().gc();
		
		String hydrotopString=ServiceTools.xslTransform(xmlFile,xslFile);
		System.out.println("  transformation ready");
		
		File hydrotopFile=new File(exportFileName+".hyd");
		System.out.println(" writing to file: "+hydrotopFile.toString());
		FileWriter out=new FileWriter(hydrotopFile);
		out.write(hydrotopString);
		out.close();
		*/
		

	    }
	catch(Exception e)
	    {
		System.out.println("could not write hydrotopfile");
		e.printStackTrace();
	    }
	try
	    {
		State wcState=new State();
		wcState.xmlParser(xmlSource);
		Node rootNode=wcState.getNodeFromNr( rootNodeNumber.intValue());
		if( rootNode == null )
		    {
			throw new KalypsoFilterException(
							 "The root node " + rootNodeNumber
							 + " has not been found!", KalypsoFilterException.INVALID_ELEMENT );
		    }
		State subState= wcState.createSubState( rootNode );
		//subState.m_rbs.writeConRbToRbToFile("c:\\temp\\worked.dat" );
		subState.writeStateToFile( exportFileName, subState.getStrandFromNodeWc( rootNode ) );
		
		
		allClimateFiles.addAll(subState.getAllFileClimate());
		allShortTermFiles.addAll(subState.getAllFileShortTerm());
		allLongTermFiles.addAll(subState.getAllFileLongTerm());
		LogFile.log( "Kalypso-ASCII files have been successfully written to " + exportFileName );
		subState.stateStatsToLogFile();
		System.out.println( "END OF PROCESS!" );
	    }
	catch(Exception e)
	    {
		System.out.println( "Sorry, import was not successfull, because: " + e.getMessage());
		LogFile.log( "Import not successful: " + e.getMessage());
		throw e;
	    }
	finally
	    {
		closeFilter();
	    }
    }

    public static File[] getInputFiles( String title )
    {
		JFileChooser chooser = new JFileChooser( new File("."));
		chooser.setMultiSelectionEnabled( true );
		chooser.showDialog( null, title );
		return chooser.getSelectedFiles();
    } // getFileName
    
	
	public Hashtable importResulsts( File resSource, File resTarget, String exportPattern, String dataSeperator ) throws Exception
	{
		Timeseries ts = Timeseries.createTS( resSource.getName() );
		ts.setExportSDF( exportPattern );
		ts.setTSseperator( dataSeperator );
		
		Hashtable ht = new Hashtable();
		try
		{
			System.out.println( "Importing " + resSource.getName() + " result file ..." );
			ts.importResultFile( resSource );
			LogFile.log( "The import of " + resSource.getName() + " was successful!" );
			System.out.println( "The import of " + resSource.getName() + " was successful!" );
			
			System.out.println( "The export is starting ..." );
			ht = ts.exportTS( resTarget );
			LogFile.log( "The export to" + resTarget + " was successful!: END OF LOG FILE" );
			System.out.println( "The export was successful: END OF PROCESS" );
		}
		catch ( Exception e )
		{
			LogFile.log( "Error Message: " + e );
			System.out.println(" Error Message: " + e);
			throw e;
		}
		return ht;
	}
	public static void closeFilter()
	{
		LogFile.log( "END OF LOGFILE" );
		LogFile.close();
		System.out.println( "Exiting Filter" );
		//		System.exit( 0 );
	}
}

