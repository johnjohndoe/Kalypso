package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.FeatureHelper;

public class NAControlConverter
{
  //  private static final String NL = System.getProperty( "line.separator" );

  // graphicTool: types
  public static final int LINE = 0;

  public static final int BLOCK = 1;

  public static final int P = 2;

  public static final int M = 3;

  public static final int T = 4;

  // graphictool: sides for y-axis
  public static final int LEFT = 0;

  public static final int RIGHT = 1;

  public static void featureToASCII( File projectPath,Feature metaFE, GMLWorkspace controlWorkspace,
      GMLWorkspace modellWorkspace ) throws IOException
  {
    final Feature controlFE = controlWorkspace.getRootFeature();
    final File startDir = new File( projectPath, "start" );
    startDir.mkdirs();

    final File startFile = new File( startDir, "we_nat_start.txt" );
    final File falStartFile = new File( startDir, "falstart.lst" );

    // write FalStart
    final StringBuffer b1 = new StringBuffer();
    writeFalstart( metaFE,startFile, b1 );
    //write it
    final FileWriter writer1 = new FileWriter( falStartFile );
    writer1.write( b1.toString() );
    writer1.close();

    // generate Start
    final StringBuffer b = new StringBuffer();
    appendResultsToGenerate( controlFE, b );
    appendResultInformation( modellWorkspace,controlWorkspace, b );
    //write it
    final FileWriter writer = new FileWriter( startFile );
    writer.write( b.toString() );
    writer.close();
  }

  private static void appendResultsToGenerate( Feature controlFE, StringBuffer b )
  {
    b.append( " " + FeatureHelper.getAsString( controlFE, "timeStep" ) + "\n" );

    b.append( getBoolean( controlFE.getProperty( "tmp" ) )
        + "       Temperatur                 .tmp\n" );
    b.append( getBoolean( controlFE.getProperty( "pre" ) )
        + "       Niederschlag               .pre\n" );
    b.append( getBoolean( controlFE.getProperty( "sch" ) )
        + "       Schnee                     .sch\n" );
    b.append( getBoolean( controlFE.getProperty( "bof" ) )
        + "       Bodenfeuchte               .bof\n" );
    b.append( getBoolean( controlFE.getProperty( "bsp" ) )
        + "       Bodenspeicher              .bsp\n" );
    b.append( getBoolean( controlFE.getProperty( "gws" ) )
        + "       Grundwasserstand           .gws\n" );
    b.append( getBoolean( controlFE.getProperty( "qgs" ) )
        + "       Gesamtabfluss Knoten       .qgs\n" );
    b.append( getBoolean( controlFE.getProperty( "qgg" ) )
        + "       Gesamtabfluss TG           .qgg\n" );
    b.append( getBoolean( controlFE.getProperty( "qna" ) )
        + "       Oberflaechenabfluss        .qna\n" );
    b.append( getBoolean( controlFE.getProperty( "qif" ) )
        + "       Interflow                  .qif\n" );
    b.append( getBoolean( controlFE.getProperty( "qvs" ) )
        + "       Abfluss vers. Flaechen     .qvs\n" );
    b.append( getBoolean( controlFE.getProperty( "qbs" ) )
        + "       Basisabfluss               .qbs\n" );
    b.append( getBoolean( controlFE.getProperty( "qt1" ) )
        + "       Kluftgrundw1               .qt1\n" );
    b.append( getBoolean( controlFE.getProperty( "qtg" ) )
        + "       Kluftgrundw                .qtg\n" );
    b.append( getBoolean( controlFE.getProperty( "qgw" ) )
        + "       Krundwasser                .qgw\n" );
    b.append( getBoolean( controlFE.getProperty( "kap" ) )
        + "       Kapil.Aufstieg/Perkolation .kap\n" );
    b.append( getBoolean( controlFE.getProperty( "vet" ) )
        + "       Evapotranspiration         .vet\n" );
    b.append( getBoolean( controlFE.getProperty( "hyd" ) )
        + "       Ausgabe hydrotope          .hyd\n" );
    b.append( getBoolean( controlFE.getProperty( "bil" ) )
        + "       Abflussbilanz              .bil\n" );
    b.append( getBoolean( controlFE.getProperty( "nmq" ) )
        + "       Statistische Abflusswerte  .nmq\n" );
    b.append( getBoolean( controlFE.getProperty( "spi" ) )
        + "       Speicherinhalt             .spi\n" );
    b.append( getBoolean( controlFE.getProperty( "sub" ) )
        + "       Speicherueberlauf          .sup\n" );
    b.append( getBoolean( controlFE.getProperty( "sph" ) )
        + "       Wasserstand Speicher       .sph\n" );
    b.append( getBoolean( controlFE.getProperty( "spv" ) )
        + "       Talsperrenverdunstung      .spv\n" );
    b.append( getBoolean( controlFE.getProperty( "spn" ) )
        + "       Zehrung                    .spn\n" );
    b.append( getBoolean( controlFE.getProperty( "vep" ) )
        + "       Evaporation                .vep\n" );
  }

  private static void appendResultInformation(GMLWorkspace modellWorkspace, GMLWorkspace controlWorkspace, StringBuffer b )
  {
    // knoten
    final FeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
    final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
    boolean onlyRootNodeResult=FeatureHelper.booleanIsTrue(controlWorkspace.getRootFeature(),"resultForRootNodeOnly",true);
    final String rootNodeID=(String)controlWorkspace.getRootFeature().getProperty("rootNode");
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      // fuer root node immer ein ergebnis generieren
      if(rootNodeID.equals(nodeFEs[i].getId()))
        b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
      // fuer nicht root node nur ergebnisse generieren wenn gewuenscht
      else if( !onlyRootNodeResult && FeatureHelper.booleanIsTrue( nodeFEs[i], "generateResult", false ) )
        b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
    }
    b.append( "99999\n" );
    // teilgebiete
    final FeatureType catchmentFT = modellWorkspace.getFeatureType( "Catchment" );
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( catchmentFT );
    for( int i = 0; i < catchmentFEs.length; i++ )
    {
      if( FeatureHelper.booleanIsTrue( catchmentFEs[i], "generateResult", false ) )
        b.append( FeatureHelper.getAsString( catchmentFEs[i], "inum" ) + "\n" );
    }
    b.append( "99999\n" );
    // TODO startwerte fuer die kurzzeitsimulation
    b.append( "99999\n" );
  }

  private static void writeFalstart( Feature metaFE,File startFile,
      StringBuffer b )
  {

    String system = "we";//"sys";
    String zustand = "nat";

    String startDate = FeatureHelper.getFormatedDate( metaFE, "startsimulation",
        "yyyy MM dd HH", "notset" );
    String endDate = FeatureHelper.getFormatedDate( metaFE, "startforecast", "yyyy MM dd HH",
        "notset" );
    // TODO set endsimulation, see also NaModellConverter
    //    String endDate = FeatureHelper.getFormatedDate( metaFE, "endsimulation", "yyyy MM dd HH",
//        "notset" );

    b.append( "xxx\n" );
    b.append( "x einzugsgebiet\n" );
    b.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad\n" );
    b.append( "2 .. " + system + " " + zustand + "  " + startDate + " " + endDate + " " + "start"
        + File.separator + startFile.getName() + "\n" );
  }

  private static String getBoolean( Object object )
  {
    if( object == null || ( !( object instanceof Boolean ) ) )
      return "n";
    boolean flag = ( (Boolean)object ).booleanValue();
    if( flag )
      return "j";
    return "n";
  }

  //    public static void renameOutputFiles(File outDir)
  //    {
  //        System.out.println("rename result files in " + outDir.toString());
  //        FileSystemUtils.move(outDir, "tmp.dat", "temperature.dat");
  //        FileSystemUtils.move(outDir, "pre.dat", "precipitation.dat");
  //        FileSystemUtils.move(outDir, "sch.dat", "snow.dat");
  //        FileSystemUtils.move(outDir, "bof.dat", "soil_moisture.dat");
  //        FileSystemUtils.move(outDir, "bsp.dat", "soil_moisture_balance.dat");
  //        FileSystemUtils.move(outDir, "qws.dat", "ground_water_heigth.dat");
  //        FileSystemUtils.move(outDir, "qgs.dat", "node_discharge.dat");
  //        FileSystemUtils.move(outDir, "qgg.dat", "discharge_catchment.dat");
  //        FileSystemUtils.move(outDir, "qna.dat", "surface_flow_natural.dat");
  //        FileSystemUtils.move(outDir, "qvs.dat", "surface_flow_sealed.dat");
  //        FileSystemUtils.move(outDir, "qif.dat", "interflow.dat");
  //        FileSystemUtils.move(outDir, "qbs.dat", "baseflow.dat");
  //        FileSystemUtils.move(outDir, "qgw.dat", "ground_water_discharge.dat");
  //        FileSystemUtils.move(outDir, "spi.dat", "storage_volume.dat");
  //        FileSystemUtils.move(outDir, "sub.dat", "storage_output.dat");
  //        FileSystemUtils.move(outDir, "sph.dat", "storage_water_table.dat");
  //    }

}