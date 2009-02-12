package de.tuhh.wb.javagis.simulation;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.tuhh.wb.javagis.FileSystemUtils;
import de.tuhh.wb.javagis.view.LogView;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.VectorSet;


public class KonfigWrite
{
    // graphicTool: types
    public static final int LINE=0;
    public static final int BLOCK=1;
    public static final int P=2;
    public static final int M=3;
    public static final int T=4;

    // graphictool: sides for y-axis 
    public static final int LEFT=0;
    public static final int RIGHT=1;

    public KonfigWrite()
    {
    }
    
    public  void writeKonfig(File konfigFile, GisTransferObject gto) throws IOException
    {
	FileWriter out=new FileWriter(konfigFile);
	writeln(out," "+gto.getSimpleProperty("m_timeStep"));
	//	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_tmp"),"       temperatur                 .tmp");
	//	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_pre"),"       Niederschlag               .pre");
	//	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qgs"),"       gesamtabfluss              .qgs");
	//	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qgg"),"       Gesamtabfluss TG           .qgg");
	writeBoolean(out,true,"       Temperatur                 .tmp");
	writeBoolean(out,true,"       Niederschlag               .pre");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_sch"),"       Schnee                     .sch");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_bof"),"       Bodenfeuchte               .bof");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_bsp"),"       Bodenspeicher              .bsp");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_gws"),"       Grundwasserstand           .gws");
	writeBoolean(out,true,"       Gesamtabfluss Knoten       .qgs");
	writeBoolean(out,true,"       Gesamtabfluss TG           .qgg");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qna"),"       Oberflaechenabfluss        .qna");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qif"),"       Interflow                  .qif");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qvs"),"       Abfluss vers. Flaechen     .qvs");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qbs"),"       Basisabfluss               .qbs");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qt1"),"       Kluftgrundw1               .qt1");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qtg"),"       Kluftgrundw                .qtg");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_qgw"),"       Krundwasser                .qgw");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_kap"),"       Kapil.Aufstieg/Perkolation .kap");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_vet"),"       Evapotranspiration         .vet");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_hyd"),"       Ausgabe hydrotope          .hyd");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_bil"),"       Abflussbilanz              .bil");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_nmq"),"       Statistische Abflusswerte  .nmq");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_spi"),"       Speicherinhalt             .spi");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_sub"),"       Speicherueberlauf          .sup");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_sph"),"       Wasserstand Speicher       .sph");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_spv"),"       Talsperrenverdunstung      .spv");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_spn"),"       Zehrung                    .spn");
	writeBoolean(out,gto.getSimplePropertyAsBoolean("m_vep"),"       Evaporation                .vep");
	
	VectorSet knoten=gto.getVectorSet("m_knotenNummern");
	if(knoten!=null)
	    {
		for(int row=0;row<knoten.size();row++)
		    writeln(out,knoten.getSimpleProperty("v_nnr",row));
	    }
	writeln(out,"99999");
	VectorSet teilgebiete=gto.getVectorSet("m_teilgebiete");
	if(teilgebiete!=null)
	    {
		for(int row=0;row<teilgebiete.size();row++)
		    writeln(out," "+teilgebiete.getSimpleProperty("v_tgn",row));
	    }
	writeln(out,"99999");
	//TODO Vectorset Startwerte (JH)
	// do not store long term results
	writeln(out,"99999");	
	if(gto.getSimpleProperty("lzpath")!=null)
	    {
		File lzpath=new File(new File(gto.getSimpleProperty("lzpath")),"lzsim");
		if(lzpath.exists())
		    {
			writeln(out,"lzpath="+gto.getSimpleProperty("lzpath"));
			LogView.println("using precalculated initial values from \""+lzpath.toString()+"\"");
		    }
		else
		    {
			LogView.println("path for initial values \""+lzpath.toString()+"\" does not exist");
			LogView.println(" using template directory instead");
		    }
	    }
	else if(gto.getSimpleProperty("file_lzpath")!=null)
	    {
		File lzpath=new File(new File(gto.getSimpleProperty("file_lzpath")),"lzsim");
		if(lzpath.exists())
		    {
			writeln(out,"lzpath="+gto.getSimpleProperty("file_lzpath"));
			LogView.println("using precalculated initial values from \""+lzpath.toString()+"\"");
		    }
		else
		    {
			LogView.println("path for initial values \""+lzpath.toString()+"\" does not exist");
			LogView.println(" using template directory instead");
		    }
	    }
	else
	    LogView.println("using template directory for initial values");
	out.close();
    }
    
    
    public void writeFalstart(File startFile,File projectPath,GisTransferObject gto) throws IOException
    {
	//	File dest=new File(FileSystemUtils.getNaWorkDir(),FalstartFileName);
	FileWriter out=new FileWriter(startFile);
	String system="tis";//"sys";
	String zustand="eik";
	//	String projectPath=FileSystemUtils.getNaWorkDir().getAbsolutePath();
	String startDate=gto.getSimplePropertyFormatedDate("m_startDate","yyyy MM dd HH");
	String endDate=gto.getSimplePropertyFormatedDate("m_endDate","yyyy MM dd HH");
	
	writeln(out,"xxx");
	writeln(out,"x einzugsgebiet");
	writeln(out,"x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad");
	writeln(out,"2 "+projectPath.getAbsolutePath()+" "+system+" "+zustand+"  "+startDate+" "+endDate+" "+"start"+File.separator+"nam.konfig");
	//	write(out,"2 d:\\nam_tisza "+system+" "+zustand+"  1998 11 03 16 1998 11 30 22 start\\NAM.konfig\n");
	out.close();	
    }
      
   public static void writeGraphicTemplate(File templateFile,File resultsDir,GisTransferObject gto)
    {
	Pattern wFilePattern=Pattern.compile(".+waterlevel_(.+)\\.dat$");
	Matcher m=null;
	try
	    {
		FileWriter out=new FileWriter(templateFile);
		int lineCount=1;
		String block;
		File file;
		// Nodes
		if(gto.getSimplePropertyAsBoolean("visualize_w"))
		    {		
			File files[]=resultsDir.listFiles();
			for(int cf=0;cf<files.length;cf++)
			    {
				System.out.println("wl: test: "+files[cf].toString());
				if(!files[cf].isDirectory())
				    {
					m=wFilePattern.matcher(files[cf].toString());
					if(m.matches())
					    {
						System.out.println("found");
						String title="water_stage(N_"+m.group(1)+")";
						writeTemplateLine(out,lineCount,files[cf],LINE,RIGHT,title,null);
						lineCount++;
					    }
				    }
			    }
		    }
		
		// catchments
		VectorSet teilgebiete=gto.getVectorSet("m_teilgebiete");
		VectorSet knoten=gto.getVectorSet("m_knotenNummern");
		if(teilgebiete!=null)
		    {
			if(gto.getSimplePropertyAsBoolean("visualize_precipitation"))
			    {
				file=new File(resultsDir,"precipitation.dat");
				if(!file.exists())
				    file=new File(resultsDir,"pre.dat");
				if(!file.exists())
				    System.out.println("could not find file "+file.toString());
				else
				    for(int row=0;row<teilgebiete.size();row++)
					{
					    block=teilgebiete.getSimpleProperty("v_tgn",row);
					    writeTemplateLine(out,lineCount,file,BLOCK,LEFT,"precipitation(C_"+block+")",block);
					    lineCount++;
					}
			    }
			if(gto.getSimplePropertyAsBoolean("visualize_temperature"))
			    {
				file=new File(resultsDir,"temperature.dat");
				if(!file.exists())
				    file=new File(resultsDir,"tmp.dat");
				if(!file.exists())
				    System.out.println("could not find file "+file.toString());
				else
				    for(int row=0;row<teilgebiete.size();row++)
					{
					    block=teilgebiete.getSimpleProperty("v_tgn",row);
					    writeTemplateLine(out,lineCount,file,LINE,LEFT,"temperature(C_"+block+")",block);
					    lineCount++;
					}
			    }
		    }		
		if(knoten!=null)
		    {
			if(gto.getSimplePropertyAsBoolean("visualize_Q"))
			    {
				file=new File(resultsDir,"node_discharge.dat");
				if(!file.exists())
				    file=new File(resultsDir,"qgs.dat");
				if(!file.exists())
				    System.out.println("could not find file "+file.toString());
				else
				    for(int row=0;row<knoten.size();row++)
					{
					    block=knoten.getSimpleProperty("v_nnr",row);
					    writeTemplateLine(out,lineCount,file,LINE,RIGHT,"Q(N_"+block+")",block);
					    lineCount++;
					}
			    }

		    }
		
		/*	
		//		  Dateien aus dem klima.dat-Ordner koennen vom graphic-tool leider nicht angezeigt werden :-(
		  
			File sequenceDir=new File(resultsDir.getParentFile(),"klima.dat");
			VectorSet rainSequences=gto.getVectorSet("rainseq");
			if(rainSequences!=null)
			{
 			for(int row=0;row<rainSequences.size();row++)
			{
			String fileName=rainSequences.getSimpleProperty("name",row);
			file=new File(sequenceDir,fileName);
			if(!file.exists())
			System.out.println("couldn not find file "+file.toString());
			else
			{
			writeTemplateLine(out,lineCount,file,LINE,LEFT,"precipitation(station_"+fileName+")"," ");
			lineCount++;
			}
			}
			}
			
			
			VectorSet tempSequences=gto.getVectorSet("tempseq");
			if(rainSequences!=null)
			{
 			for(int row=0;row<tempSequences.size();row++)
			{
			String fileName=tempSequences.getSimpleProperty("name",row);
			file=new File(sequenceDir,fileName);
			if(!file.exists())
			System.out.println("couldn not find file "+file.toString());
			else
			{
			writeTemplateLine(out,lineCount,file,LINE,RIGHT,"temperature(station_"+fileName+")"," ");
			lineCount++;
			}
			}
			}
		*/

		try
		    {
			String lineMarkString="LMarke: "+gto.getSimplePropertyFormatedDate("m_forecastDate","dd.MM.yyyy HH:mm:00");
			writeln(out,lineMarkString);  //LMarke: 14.12.1969 07:30:00
		    }
		catch(Exception e)
		    {
			System.out.println("could not set forecast-line-mark");
		    }
		writeln(out,"xTitel: Time"); 
		writeln(out,"yTitel1: Precipitation [mm/h], Temperature [C]"); 
		writeln(out,"yTitel2: Water stage [cm], Discharge [m**3/s]"); 
		writeln(out,"HTitel: Forecast Results ("+resultsDir.toString()+")"); 
		out.close();	
	    }
	catch(IOException e)
	    {
		LogView.println("\n+"+e.getMessage());
		LogView.println("could not write templatefile for graphic tool,\nplease load results form graphictool manually");
	    }
    }
    
    private static void writeTemplateLine(FileWriter writer,int num,File file,int type,int axisSide,String title,String block)
	throws IOException
    {
	String diagramString="L";
	String axisString="1";
	switch(type)
	    {
	    case T:
		diagramString="T";
		break;
	    case M:
		diagramString="M";
		break;
	    case P:
		diagramString="P";
		break;
	    case BLOCK:
		diagramString="B";
		break;
	    case LINE:
	    default:
		diagramString="L";
		break;
	    }
	switch(axisSide)
	    {
	    case RIGHT:
		axisString="2";
		break;
	    case LEFT:
	    default:
		axisString="1";
		break;
	    }
	String line;
	if(block!=null)
	    line=Integer.toString(num)+"- "+file.toString()+" J "+diagramString+" "+axisString+" "+title+" "+block;
	else
	    line=Integer.toString(num)+"- "+file.toString()+" J "+diagramString+" "+axisString+" "+title;
	writeln(writer,line);
    }

    private void writeBoolean(FileWriter writer,boolean flag,String text) throws IOException
    {
	if(flag)
	    writeln(writer,"j"+text);
	else
	    writeln(writer,"n"+text);
    }
    
    private static void write(FileWriter writer,String line) throws IOException
    {
	writer.write(line,0,line.length());
    }
    private static void writeln(FileWriter writer,String line) throws IOException
    {
	line=line+System.getProperty("line.separator");
	writer.write(line,0,line.length());
    }

    public static File moveOutputDir(File dir,String srcName,String destName)
    {
	System.out.println("move result dir");
	File srcFile=new File(dir,srcName);
	File destFile=new File(dir,destName);
	int n=0;
	try
	    {
		while(destFile.exists())
		    {
			n++;
			destFile=new File(dir,destName+"_"+Integer.toString(n));
		    }
		srcFile.renameTo(destFile);
	    }
	catch(Exception e)
	    {
		// ignore
		LogView.println("could not move results to "+destFile.toString());
		LogView.println(" try directory \"out_tis.eik\"\n");
		return srcFile;
	    }
	return destFile;
    }
    
    public static void renameOutputFiles(File outDir)
    {
	System.out.println("rename result files in "+outDir.toString());
	FileSystemUtils.move(outDir,"tmp.dat","temperature.dat");
	FileSystemUtils.move(outDir,"pre.dat","precipitation.dat");
	FileSystemUtils.move(outDir,"sch.dat","snow.dat");
	FileSystemUtils.move(outDir,"bof.dat","soil_moisture.dat");
	FileSystemUtils.move(outDir,"bsp.dat","soil_moisture_balance.dat");
	FileSystemUtils.move(outDir,"qws.dat","ground_water_heigth.dat");
	FileSystemUtils.move(outDir,"qgs.dat","node_discharge.dat");
	FileSystemUtils.move(outDir,"qgg.dat","discharge_catchment.dat");
	FileSystemUtils.move(outDir,"qna.dat","surface_flow_natural.dat");
	FileSystemUtils.move(outDir,"qvs.dat","surface_flow_sealed.dat");
	FileSystemUtils.move(outDir,"qif.dat","interflow.dat");
	FileSystemUtils.move(outDir,"qbs.dat","baseflow.dat");
	FileSystemUtils.move(outDir,"qgw.dat","ground_water_discharge.dat");
	FileSystemUtils.move(outDir,"spi.dat","storage_volume.dat");
	FileSystemUtils.move(outDir,"sub.dat","storage_output.dat");
	FileSystemUtils.move(outDir,"sph.dat","storage_water_table.dat");
    }

    public static void clearSimulationDir(File simulationDir)
    {  
	if("Linux".equals(System.getProperty("os.name")))
	    return;
	System.out.println("clear simulation dir");
	try
	    {
		FileSystemUtils.recursiveDelete(new File(simulationDir,"hydro.top"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"inp.dat"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"out_tis.eik"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"klima.dat"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"na-modell"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"start"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"tools"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"wq_tables"));
		FileSystemUtils.recursiveDelete(new File(simulationDir,"xml_temp"));
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("could not clear simulation dir complete\n");
	    }
    }

    public static Vector getFileNames(File dir,String postFix)
    {
	Vector results=new Vector();
       	if(dir.isDirectory())
	    {
		File files[]=dir.listFiles();
		for(int i=0;i<files.length;i++)
		    {
			if(!files[i].isDirectory())
			    {
				String name=(files[i].getName()).toLowerCase();
				if(postFix==null)
				    results.add(name);
				else if(name.endsWith(postFix))
				    results.add(name.substring(0,name.length()-postFix.length()));
			    }
		    }
	    }
	return results;
    }

    public static Vector q2wTransformation(File simulationDir,File resultDir)
    {
	System.out.println("start wq transformation");
	Vector resultFiles=new Vector();
	try
	    {
		File wqTableDir=new File(simulationDir,"wq_tables");
		BlockTimeSeries qBlock=new BlockTimeSeries();
		Vector wqNodes=getFileNames(wqTableDir,".wq");
		qBlock.importBlockFile(new File(resultDir,"qgs.dat"),wqNodes);
		File tempFile=File.createTempFile("q_temp",".dat");
		for(Enumeration e=qBlock.getKeys();e.hasMoreElements();)
		    {
			String key=(String)e.nextElement();
			System.out.println("KEY: "+key);
			if(tempFile.exists())
			    tempFile.delete();
			qBlock.exportToFile(key,tempFile);
			File wqTableFile=new File(wqTableDir,key+".wq");
			File resultFile=new File(resultDir,"waterlevel_N"+key+".dat");
			String args[]=
			    {
				tempFile.toString(),
				wqTableFile.toString(),
				resultFile.toString(),
				"/QW",
			    };
			String comandLine=simulationDir.getPath()+System.getProperty("file.separator")+"tools"+System.getProperty("file.separator")+"wqtrans.exe";
			FileSystemUtils.executeProcess(comandLine,args,resultDir);
			
			resultFiles.add(resultFile);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("Problems converting discharge to waterlevel\n");
	    }
	return resultFiles;
    }
}
