/*
 * Created on Oct 7, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.convert.namodel;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.deegree.model.feature.Feature;

/**
 * @author doemming
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class Configuration
{

    private final URL m_schemaURL;

//    private final URL m_modellURL;

    private final File m_catchmentFile;

    private final URL m_ChannelFormatURL;

//    private final Feature m_rootFeature;

    private final URL m_catchmentFormatURL;

    private final File m_channelFile;

    // ASCII -> GML
    public Configuration(File filePrefix) throws MalformedURLException
    {
        // schema
        m_schemaURL = getClass().getResource( "schema/namodellV3.xsd");

        // formate:
        m_catchmentFormatURL = getClass().getResource("formats/WernerCatchment.txt");
        m_ChannelFormatURL = getClass().getResource("formats/gerinne.txt");

        // ASCII
        m_catchmentFile = new File(filePrefix, "inp.dat/we_nat.geb");
        m_channelFile = new File(filePrefix,"inp.dat/we_nat.ger");        

//        m_modellURL = gmlFile;//.toURL();//getClass().getResource(prefix+"modell/namodell.gml");

        
//        m_rootFeature = null;
    }

//    public static Configuration createReadConf(File filePrefix) throws MalformedURLException
//    {
//        return null;
//    }
    
//    public Configuration(String prefix, Feature rootFeature)
//    {
//        //	String prefix="";
//        // formate:
//        m_catchmentFormatURL = getClass().getResource(
//                prefix + "formats/WernerCatchment.txt");
//        m_ChannelFormatURL = getClass().getResource(
//                prefix + "formats/gerinne.txt");
//
//        m_schemaURL = getClass().getResource(prefix + "schema/namodellV3.xsd");
//        m_modellURL = getClass().getResource(prefix + "modell/namodell.gml");
//
//        m_catchmentURL = getClass().getResource(prefix + "inp.dat/we_nat.geb");
//        m_channelURL = getClass().getResource(prefix + "inp.dat/we_nat.ger");
//
//        m_rootFeature = rootFeature;
//    }

    public URL getSchemaURL()
    {
        return m_schemaURL;
    }

    public URL getChannelFormatURL()
    {
        return m_ChannelFormatURL;
    }

    public File getChannelFile()
    {
        return m_channelFile;
    }

//    public Feature getRootFeature()
//    {
//
//        return m_rootFeature;
//    }

    public URL getCatchmentFormatURL()
    {
        return m_catchmentFormatURL;
    }

    public File getCatchmentFile()
    {
        return m_catchmentFile;
    }

//    public URL getModellURL()
//    {
//        return m_modellURL;
//    }

    
    
}