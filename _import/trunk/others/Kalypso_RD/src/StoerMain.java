
import java.io.File;
import java.io.FileReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

import org.deegree.graphics.FeatureLayer;
import org.deegree.graphics.Layer;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.graphics.MapFactory;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GM_Object_Impl;
import org.opengis.cs.CS_CoordinateSystem;

import sun.util.calendar.CalendarDate;

/*
 * Created on 04.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author kraessig
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class StoerMain
{
	 //private static String dateStr = "yyyy_MM_dd HH:mm:ss";
    private static DateFormat dateFormat = new SimpleDateFormat("yyyy_MM_dd HH:mm:ss");

	private static final String DIR_OUTPUT = "data/klima/";
    public static void main(String[] args)
    {
        // try: Anweisung mit Ausnahme zum Importieren vom Kriging-Raster sowie GIS-Daten
    	 System.out.println("KALYPSO-RD-TW, Version 1, Sept. 2004, Dipl.-Ing. Kraessig, TUHH"); 
    	 System.out.println("Prg. zur Berechnung des Gebietsniederschlags auf Tageswert Basis");
    	 System.out.println("Je Teilgebiet wird eine Zeitreihe im KALYPSO Grap-Format erstellt.");
    	 System.out.println("******************************************************************");
    	 
    	 try
        {
            //Shape-File mit Teilgebieten laden
            FeatureLayer tgLayer = getTeilgebiete(new File(
                    "data/shape/Teileinzugsgebiete"), "EPSG:31467");
            Feature[] fe = tgLayer.getAllFeatures();
            for (int i = 0; i < fe.length; i++)
            {}
            System.out.println("Shape-File mit Teilgebieten geladen");
                                                       
            // Raster mit Kriging Gewichten laden
            File blockFile = new File("data/raster/Kriging_Gewichte.txt");
            System.out.println("Kriging Raster geladen");
           
            //Systemimport abgeschlossen
                  
            //Konstruktoren für Blöcke und Stationen KZ  
            Block[] alleBlocks = BlockFactory.createBlocks(new FileReader(blockFile));
            
            //Zuordnung raster - shape    
            Teilgebiet[] teilgebiete=TeilgebietFactory.createTeilgebiete(tgLayer,alleBlocks);
            
             
            //Berechnet wird das Datum in der Zeitzone GMT+1, ohne Sommer und Winterzeit 
            //System.out.println(TimeZone.getTimeZone("GMT+1").toString());//Parameter GMT
            SimpleTimeZone timeZone=new SimpleTimeZone(0,"nSWZ");
            timeZone.getDSTSavings();
            Calendar calendar=Calendar.getInstance(timeZone);
            dateFormat.setCalendar(calendar);
//          Anfangs- und Enddatum  
            Date startDate = dateFormat.parse("1971_01_01 00:00:00");
            Date endDate = dateFormat.parse("2005_02_26 00:00:00");
            
            //5Minuten Schritte: 1440 in 5 ändern
            //Zeitschritt für Zeitschleife Zeitschritt in Minuten 24h = 1440min
            Zeitschleife z = new Zeitschleife(startDate, endDate, 1440);         
            TeilgebietsSchleife tgSchleife=new TeilgebietsSchleife(teilgebiete);
            z.run(tgSchleife);
       
            for(int i=0;i<teilgebiete.length;i++)
            {
              teilgebiete[i].writeOutput(new File(DIR_OUTPUT+"c_"+teilgebiete[i].getKey()+".Niederschlag"));
            System.out.println("Berechnung des Gebietsniederschlags für TG "+teilgebiete[i].getKey()+" abgeschlossen");
            }
            System.out.println("Berechung erfolgreich beendet für Zeitraum:");
            System.out.println(dateFormat.format(startDate)+" bis "+dateFormat.format(endDate));
       	    System.out.println("Je Teilgebiet ist eine Zeitreihe im KALYPSO Grap-Format erstellt worden.");
       
            
            
            
        } catch (Exception e)
        {
            e.printStackTrace();
        }

    }

    // GIS Import: ShapeFile Daten
    
    //    KoordinatenSysteme:
    //
    //        Der vollstaendige EPSG-Code ist die ID fuer ein Koordinatensystem.
    //        also "EPSG:4326" fuer Laengen-/Breitengrade
    //        "EPSG:31467" Gauss-Krueger Z3 ist das in Deutschland gebraeuchliche.
    //        "EPSG:4284" Pulkovo wurde frueher in der DDR benutzt
    //
    //          EPSG:4326 WGS84 lat/lon
    //          EPSG:31467 Gauss-Krueger Zone 3
    //          
    //          EPSG:4004 Bessel 1841
    //          EPSG:4284 Pulkovo 1942
    //
    //          EPSG:31492 old Gauss-Krueger Zone 2
    //          EPSG:31493 old Gauss-Krueger Zone 3
    //          EPSG:31494 old Gauss-Krueger Zone 4
    //          EPSG:31495 old Gauss-Krueger Zone 5
    //
    //          EPSG:31466 Gauss-Krueger Zone 2
    //          EPSG:31467 Gauss-Krueger Zone 3
    //          EPSG:31468 Gauss-Krueger Zone 4
    //          EPSG:31469 Gauss-Krueger Zone 5
    //                                            
    //          EPSG:4230E D50
    //          EPSG:4231E D87
    //          EPSG:4258E TRS89
    //          EPSG:4314D HDN deutsches Hauptdreiecksnetz

  
    
   public static FeatureLayer getTeilgebiete(File sourceFile, String sourceCrs)
            throws Exception
    {
        final ShapeFile sf = new ShapeFile(sourceFile.getAbsolutePath());
        final FeatureFactory fFac = new FeatureFactory();
        final int count = sf.getRecordNum();

        final FeatureType featureType = sf.getFeatureByRecNo(1)
                .getFeatureType();
        //final String name = source + featureType.getName();

        final FeatureCollection collection = fFac.createFeatureCollection(
                "teilgebiete", count);
              
        
        // die shape-api liefert stets WGS84 als Koordinatensystem, daher
        // Anpassung hier:

        final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();

        final CS_CoordinateSystem srcCS = org.deegree_impl.model.cs.Adapters
                .getDefault().export(csFac.getCSByName(sourceCrs));
        //      final int max= count < 20 ? count:20;
        final int max = count; // < 20 ? count:20;
        if (max < count)
            System.out.println("WARNUNG es werden nur " + max + " von " + count
                    + " Features geladen");

        for (int i = 0; i < max; i++)
        {
            final Feature fe = sf.getFeatureByRecNo(i + 1);
            setCrs(fe, srcCS);
            if (fe != null)
                collection.appendFeature(fe);

        }
        sf.close();
        final Layer layer = MapFactory.createFeatureLayer("teilgebiete", srcCS,
                collection);
        return (FeatureLayer) layer;
    }

    public static void setCrs(Feature fe, CS_CoordinateSystem srcCS)
    {
        final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
        for (int i = 0; i < ftp.length; i++)
        {
            Object prop = fe.getProperty(ftp[i].getName());
            if (prop != null && prop instanceof Feature)
                setCrs((Feature) prop, srcCS);
            else if (prop != null && prop instanceof GM_Object)
            {
                ((GM_Object_Impl) prop).setCoordinateSystem(srcCS);
            }
        }
    }

    /**
     * ueperprueft die koordinatensysteme der geometrien, fall null, dann wird
     * das defaultCoordinatessystem angenommen (gesetzt).
     */
    public static void checkCrs(Feature fe, CS_CoordinateSystem defaultCS)
    {
        final FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
        for (int i = 0; i < ftp.length; i++)
        {
            Object prop = fe.getProperty(ftp[i].getName());
            if (prop != null && prop instanceof Feature)
                checkCrs((Feature) prop, defaultCS);
            else if (prop != null && prop instanceof GM_Object)
            {
                GM_Object_Impl gmlProp = (GM_Object_Impl) prop;
                if (gmlProp.getCoordinateSystem() == null)
                    gmlProp.setCoordinateSystem(defaultCS);
            }
        }
    }
}