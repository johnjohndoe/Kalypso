package org.kalypso.dwd.raster;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.model.sort.SplitSort;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 * 
 * Raster DWD Reader
 * 
 * 
 * Aufbau Kennsatz : -----------------
 * 
 * Spalte 1- 11: Datum der Daten YYMMDDHHhh (1X,I10)
 * 
 * 12-15 : Elementkennung der Daten (i4) hier : 114 : geogr. Breite in
 * Grad*100000 des Gittermittelpunktes 115 : geogr. Laenge in Grad*100000 des
 * Gittermittelpunktes 8 : geogr. Hoehe in m des Gittermittelpunktes
 * 
 * Aufbau Daten : --------------
 * 
 * Die Werte der im Kennsatz definierten Elemente an den Gitterpunkten in der
 * angegebenen Dimension. Die Gitterwerte sind hintereinander zeilenweise von
 * West nach Ost und von Sued nach Nord angeordnet .
 * 
 * Jeweils 13 Werte pro Zeile (13i8):
 * 
 * Spalte 1- 8 : Gitterwert 1 z.B. 4908634 bedeutet 49.08634 Grad geogr. Breite
 * 9-16 : Gitterwert 2 4908878 49.08878 Aufbau_LMFiles.txt
 */
public class Raster2ZML
{
    public static boolean TEST_SCENARIO = false;

    private final File m_ascciZmlDir;

    private final HashMap m_store;

    public static final int KEY_RAIN = 424;

    public static final int KEY_SNOW = 425;

    public static final int KEY_HEIGHT = 8;

    public static final int KEY_100000_LAT = 114;

    public static final int KEY_100000_LON = 115;

    private static final int KEY_GEO_BBOX_4326 = -4326;

    private final String DATUM = "([0-9]{10})";

    private final String KEY = "([0-9]+)";

    private final String STUNDE = "([0-9]+)";

    private final Pattern HEADER_STATIC = Pattern.compile(" " + DATUM + " +"
            + KEY);

    private final Pattern HEADER_DYNAMIC = Pattern.compile(" " + DATUM + " +"
            + KEY + " +" + STUNDE);

    //            "YYMMDDHHhh");
    private static final SimpleDateFormat m_rasterDF = new SimpleDateFormat(
            "yyMMddHHmm");

    private static final SimpleDateFormat m_zmlDF = new SimpleDateFormat(
            "yyyyMMddHHmm");

    // size of a raster cell (EPSG:4326)
    private final double rasterDY = 0.282090923076925;

    private final double rasterDX = 0.09688948027718162;

    private static final int KEY_HIGH_RESOTUTION = -9971;

    private static final int KEY_GEO_Point_4326 = -9970;


    private static final int FAKE_DENSITY = 10;

    private static DecimalFormat m_decimalFormat = new DecimalFormat();

    static
    {
        DecimalFormatSymbols dfs = new DecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        m_decimalFormat.setDecimalFormatSymbols(dfs);
        m_decimalFormat.setMaximumFractionDigits(4);
        m_decimalFormat.setMinimumIntegerDigits(1);
    }

    public Raster2ZML(File asciiZmlDir)
    {
        m_ascciZmlDir = asciiZmlDir;
        m_store = new HashMap();
    }

    public void loadRaster(LineNumberReader reader) throws IOException,
            ParseException
    {
        String line = null;
        DWDRaster raster = null;
        while ((line = reader.readLine()) != null)
        {
            Matcher dynamicHeaderMatcher = HEADER_DYNAMIC.matcher(line);
            if (dynamicHeaderMatcher.matches())
            {
                System.out.println(line);
                storeRaster(raster);
                final Date date = m_rasterDF.parse(dynamicHeaderMatcher
                        .group(1));
                final int key = Integer.parseInt(dynamicHeaderMatcher.group(2));
                final long hour = Long.parseLong(dynamicHeaderMatcher.group(3));
                Date forecastDate = new Date(date.getTime() + 60 * 60 * 1000
                        * hour);
                raster = new DWDRaster(forecastDate, key);
                continue;
            }
            Matcher staticHeaderMatcher = HEADER_STATIC.matcher(line);
            if (staticHeaderMatcher.matches())
            {
                System.out.println(line);
                storeRaster(raster);
                final Date date = m_rasterDF
                        .parse(staticHeaderMatcher.group(1));
                final int key = Integer.parseInt(staticHeaderMatcher.group(2));
                raster = new DWDRaster(date, key);
                continue;

            }
            final String[] values;
             if(raster.getKey()==KEY_RAIN && TEST_SCENARIO)
               values=line.trim().replaceAll("[0-9]+", "100").split(" +", 13);
             else
            values = (line.trim()).split(" +", 13);
            
            if (raster != null)
            {
              raster.addValues(values);
            }
            }
        storeRaster(raster);
    }

    private void storeRaster(DWDRaster raster)
    {
        if (raster == null)
            return;
        int key = raster.getKey();
        final Integer storeKey = new Integer(key);
        switch (key)
        {
        case KEY_RAIN:
        case KEY_SNOW:
            if (!m_store.containsKey(storeKey))
                m_store.put(storeKey, new ArrayList());
            ((List) m_store.get(storeKey)).add(raster);
            break;
        default:
            m_store.put(storeKey, raster);
            break;

        }
    }

    public boolean createZML(Feature[] fe) throws Exception
    {
        boolean result=true;
        final SplitSort featureSort = new SplitSort();
        final HashMap f2rasterPos = new HashMap();
        for (int i = 0; i < fe.length; i++)
        {
            f2rasterPos.put(fe[i], new ArrayList());
            featureSort.add(fe[i]);
        }
        final DWDRaster bbox_4326_Raster = (DWDRaster) m_store.get(new Integer(
                KEY_GEO_BBOX_4326));
        Object o = m_store.get(new Integer(KEY_GEO_Point_4326));
        final DWDRaster point_4326_Raster = (DWDRaster) o;
        final GeoTransformer transformer = new GeoTransformer(fe[0]
                .getDefaultGeometryProperty().getCoordinateSystem());
        final DWDRaster bbox_GK_Raster = transformRaster(bbox_4326_Raster,
                transformer);
        final DWDRaster fake_GK_Raster = createFakeRaster(point_4326_Raster,
                FAKE_DENSITY, transformer);

        System.out.println("calculate raster to catchment mapping");
        for (int i = 0; i < bbox_GK_Raster.size(); i++)
        {
            final GM_Surface bbox = (GM_Surface) bbox_GK_Raster.getElementAt(i);
            final List featureList = featureSort.query(bbox.getEnvelope(),
                    new ArrayList());
            if(featureList.size()>0)
              System.out.println(featureList.size()+" catchmets intersect raster");
            for (Iterator iter = featureList.iterator(); iter.hasNext();)
            {
                final Feature feature = (Feature) iter.next();
                // for those catchments that will not be catched by the
                // rasterpoints
                ((List) f2rasterPos.get(feature)).add(new Integer(i));
                final List pointList = (List) fake_GK_Raster.getElementAt(i);
                for (Iterator iterator = pointList.iterator(); iterator
                        .hasNext();)
                {
                    final GM_Point testPoint = (GM_Point) iterator.next();
                    if (feature.getDefaultGeometryProperty()
                            .contains(testPoint))
                        ((List) f2rasterPos.get(feature)).add(new Integer(i));
                }
            }
        }
        Set set = f2rasterPos.keySet();
        
        for (Iterator iter = set.iterator(); iter.hasNext();)
        {
            Feature feature = (Feature) iter.next();
            List posList = (List) f2rasterPos.get(feature);
            if(!createTimserie(feature.getId(), posList))
              result=false;
        }
        return result;
    }

    private DWDRaster createFakeRaster(DWDRaster pointGeoRaster, int max,
            GeoTransformer transformer) throws Exception
    {
      System.out.println("increase resolution of raster");
        double dx = rasterDX / (2d *  max);
        double dy = rasterDY / (2d * max);
        final DWDRaster result = new DWDRaster(pointGeoRaster.getDate(),
                KEY_HIGH_RESOTUTION);
        for (int i = 0; i < pointGeoRaster.size(); i++)
        {
            final GM_Point point = (GM_Point) pointGeoRaster.getElementAt(i);
            final double px = point.getX();
            final double py = point.getY();
            if (i % 100 == 0)
                System.out.print(i + "/" + pointGeoRaster.size() + "\n");
            final List col = new ArrayList();
            for (int n = 0; n < max; n++)
            {
                col.add(transformer.transform(GeometryFactory.createGM_Point(px
                        + dx * n, py, point.getCoordinateSystem())));
                col.add(transformer.transform(GeometryFactory.createGM_Point(px
                        - dx * n, py, point.getCoordinateSystem())));
                col.add(transformer.transform(GeometryFactory.createGM_Point(
                        px, py + dy * n, point.getCoordinateSystem())));
                col.add(transformer.transform(GeometryFactory.createGM_Point(
                        px, py - dy * n, point.getCoordinateSystem())));
            }
            result.addValue(col);
        }
        return result;
    }

    private boolean createTimserie(String id, List posList) throws IOException
    {
        System.out.println("Feature: " + id + " has " + posList.size()
                + " rasterpoints");
        final SortedMap zr = new TreeMap();
        createTimserie(zr, posList, KEY_RAIN);
        createTimserie(zr, posList, KEY_SNOW);
        StringBuffer csvBuffer = new StringBuffer();
        Set set = zr.keySet();
        for (Iterator iter = set.iterator(); iter.hasNext();)
        {
            final Date date = (Date) iter.next();
            final Double value = (Double) zr.get(date);            
            final double niederschlag=value.doubleValue()
            / 100d;
            csvBuffer.append(m_zmlDF.format(date) + "," +m_decimalFormat.format(niederschlag) + "\n");
        }
        final File ascciZmlFile = new File(m_ascciZmlDir, id + ".csv");
        if (ascciZmlFile.exists())
            ascciZmlFile.delete();
        
        final FileWriter writer = new FileWriter(ascciZmlFile);
        writer.write(csvBuffer.toString());
        writer.close();
        return true;              
    }

    private void createTimserie(SortedMap timeserie, List posList, int rasterKey)
    {
        final List rasters = (List) m_store.get(new Integer(rasterKey));
        for (Iterator iter = rasters.iterator(); iter.hasNext();)
        {
            final DWDRaster raster = (DWDRaster) iter.next();
            double value = 0d;
            for (Iterator i2 = posList.iterator(); i2.hasNext();)
            {
                int pos = ((Integer) i2.next()).intValue();
                final String rValue = (String) raster.getElementAt(pos);
                final double d = Double.parseDouble(rValue);
                value += d;
            }
            final Date date = raster.getDate();
            final double niederschlag = value /  posList.size();
            if (timeserie.containsKey(date))
            {
                double newValue = niederschlag
                        + ((Double) timeserie.get(date)).doubleValue();
                timeserie.put(date, new Double(newValue));
            } else
                timeserie.put(raster.getDate(), new Double(niederschlag));
        }
    }

    private DWDRaster transformRaster(DWDRaster geoRaster,
            GeoTransformer transformer) throws Exception
    {
      System.out.println("transforming coordinates");
        final DWDRaster result = new DWDRaster(geoRaster.getDate(), 0);
        for (int i = 0; i < geoRaster.size(); i++)
        {
            GM_Object geom = (GM_Object) geoRaster.getElementAt(i);
            result.addValue(transformer.transform(geom));
        }
        return result;
    }

    public void createGeoRaster() throws GM_Exception
    {
      System.out.println("create geometries from raster data");
        DWDRaster rLat = (DWDRaster) m_store.get(new Integer(KEY_100000_LAT));
        DWDRaster rLon = (DWDRaster) m_store.get(new Integer(KEY_100000_LON));
        int size = rLat.size();

        final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
        final CS_CoordinateSystem csLatLon = org.deegree_impl.model.cs.Adapters
                .getDefault().export(csFac.getCSByName("EPSG:4326"));
        final DWDRaster geoRaster = new DWDRaster(rLat.getDate(),
                KEY_GEO_BBOX_4326);
        final DWDRaster pointRaster = new DWDRaster(rLat.getDate(),
                KEY_GEO_Point_4326);
        storeRaster(geoRaster);
        storeRaster(pointRaster);
        for (int i = 0; i < size; i++)
        {
            double x = (Integer.parseInt(rLon.getElementAt(i).toString())) / 100000d;
            double y = (Integer.parseInt(rLat.getElementAt(i).toString())) / 100000d;
            final GM_Point point = GeometryFactory.createGM_Point(x, y,
                    csLatLon);
            pointRaster.addValue(point);
            double minx = x - rasterDX / 2d;
            double maxx = x + rasterDX / 2d;
            double miny = y - rasterDY / 2d;
            double maxy = y + rasterDY / 2d;
            GM_Surface box = GeometryFactory.createGM_Surface(GeometryFactory
                    .createGM_Envelope(minx, miny, maxx, maxy), csLatLon);
            geoRaster.addValue(box);
        }
    }
}