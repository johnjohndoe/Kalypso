package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.FeatureHelper;

public class KonfigWrite
{
    private static final String NL = System.getProperty("line.separator");

    // graphicTool: types
    public static final int LINE = 0;

    public static final int BLOCK = 1;

    public static final int P = 2;

    public static final int M = 3;

    public static final int T = 4;

    // graphictool: sides for y-axis
    public static final int LEFT = 0;

    public static final int RIGHT = 1;

    public static void featureToASCII(File konfigFile, Feature fe)
            throws IOException
    {
        FileWriter out = new FileWriter(konfigFile);

        StringBuffer b = new StringBuffer();
        appendResultsToGenerate(fe, b);
        out.write(b.toString());
    }

    private static void appendResultsToGenerate(Feature fe, StringBuffer b)
    {
        b.append(" " + fe.getProperty("timeStep") + "\n");

        b.append(getBoolean(fe.getProperty("tmp"))
                + "       Temperatur                 .tmp\n");
        b.append(getBoolean(fe.getProperty("pre"))
                + "       Niederschlag               .pre\n");
        b.append(getBoolean(fe.getProperty("sch"))
                + "       Schnee                     .sch\n");
        b.append(getBoolean(fe.getProperty("bof"))
                + "       Bodenfeuchte               .bof\n");
        b.append(getBoolean(fe.getProperty("bsp"))
                + "       Bodenspeicher              .bsp\n");
        b.append(getBoolean(fe.getProperty("gws"))
                + "       Grundwasserstand           .gws\n");
        b.append(getBoolean(fe.getProperty("qgs"))
                + "       Gesamtabfluss Knoten       .qgs\n");
        b.append(getBoolean(fe.getProperty("qgg"))
                + "       Gesamtabfluss TG           .qgg\n");
        b.append(getBoolean(fe.getProperty("qna"))
                + "       Oberflaechenabfluss        .qna\n");
        b.append(getBoolean(fe.getProperty("qif"))
                + "       Interflow                  .qif\n");
        b.append(getBoolean(fe.getProperty("qvs"))
                + "       Abfluss vers. Flaechen     .qvs\n");
        b.append(getBoolean(fe.getProperty("qbs"))
                + "       Basisabfluss               .qbs\n");
        b.append(getBoolean(fe.getProperty("qt1"))
                + "       Kluftgrundw1               .qt1\n");
        b.append(getBoolean(fe.getProperty("qtg"))
                + "       Kluftgrundw                .qtg\n");
        b.append(getBoolean(fe.getProperty("qgw"))
                + "       Krundwasser                .qgw\n");
        b.append(getBoolean(fe.getProperty("kap"))
                + "       Kapil.Aufstieg/Perkolation .kap\n");
        b.append(getBoolean(fe.getProperty("vet"))
                + "       Evapotranspiration         .vet\n");
        b.append(getBoolean(fe.getProperty("hyd"))
                + "       Ausgabe hydrotope          .hyd\n");
        b.append(getBoolean(fe.getProperty("bil"))
                + "       Abflussbilanz              .bil\n");
        b.append(getBoolean(fe.getProperty("nmq"))
                + "       Statistische Abflusswerte  .nmq\n");
        b.append(getBoolean(fe.getProperty("spi"))
                + "       Speicherinhalt             .spi\n");
        b.append(getBoolean(fe.getProperty("sub"))
                + "       Speicherueberlauf          .sup\n");
        b.append(getBoolean(fe.getProperty("sph"))
                + "       Wasserstand Speicher       .sph\n");
        b.append(getBoolean(fe.getProperty("spv"))
                + "       Talsperrenverdunstung      .spv\n");
        b.append(getBoolean(fe.getProperty("spn"))
                + "       Zehrung                    .spn\n");
        b.append(getBoolean(fe.getProperty("vep"))
                + "       Evaporation                .vep\n");
    }

    public void appendResultNodes(GMLWorkspace workspace, StringBuffer b)
    {
        // knoten
        final FeatureType nodeFT = workspace.getSchema().getFeatureType(
                "knoten");
        final Feature[] nodeFEs = workspace.getFeatures(nodeFT);
        for (int i = 0; i < nodeFEs.length; i++)
        {
            if (FeatureHelper
                    .booleanIsTrue(nodeFEs[i], "generateResult", false))
                b.append(nodeFEs[i].getProperty("num").toString() + "\n");
        }
        b.append("99999\n");
        // teilgebiete
        final FeatureType catchmentFT = workspace.getSchema().getFeatureType(
                "knoten");
        final Feature[] catchmentFEs = workspace.getFeatures(catchmentFT);
        for (int i = 0; i < catchmentFEs.length; i++)
        {
            if (FeatureHelper.booleanIsTrue(catchmentFEs[i], "generateResult",
                    false))
                b.append(catchmentFEs[i].getProperty("num").toString() + "\n");
        }
        b.append("99999\n");
        // TODO startwerte fuer die kurzzeitsimulation
        b.append("99999\n");
    }

    public void writeFalstart(File falStartFile, File projectPath, Feature fe)
    {
        //	File dest=new File(FileSystemUtils.getNaWorkDir(),FalstartFileName);
        //        FileWriter out = new FileWriter(startFile);
        StringBuffer b = new StringBuffer();
        String system = "tis";//"sys";
        String zustand = "eik";

        //	String projectPath=FileSystemUtils.getNaWorkDir().getAbsolutePath();

        String startDate = FeatureHelper.getFormatedDate(fe, "startDate",
                "yyyy MM dd HH", "notset");
        String endDate = FeatureHelper.getFormatedDate(fe, "startDate",
                "yyyy MM dd HH", "notset");

        b.append("xxx\n");
        b.append("x einzugsgebiet\n");
        b
                .append("x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad\n");
        b.append("2 " + projectPath.getAbsolutePath() + " " + system + " "
                + zustand + "  " + startDate + " " + endDate + " " + "start"
                + File.separator + "nam.konfig\n");
    }

    private static String getBoolean(Object object)
    {
        if (object == null || (!(object instanceof Boolean)))
            return "n";
        boolean flag = ((Boolean) object).booleanValue();
        if (flag)
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