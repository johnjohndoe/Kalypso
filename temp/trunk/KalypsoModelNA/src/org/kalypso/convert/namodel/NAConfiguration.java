/*
 * Created on Oct 7, 2004
 * 
 */
package org.kalypso.convert.namodel;

import java.io.File;
import java.net.URL;

/**
 * @author doemming
 * 
 */
public class NAConfiguration
{

  private final URL m_schemaURL;

  private final File m_catchmentFile;

  private final URL m_ChannelFormatURL;

  private final URL m_catchmentFormatURL;

  private final File m_channelFile;

  private final URL m_netFormatURL;

  private final File m_netFile;

  private final URL m_controlSchemaURL;

  public NAConfiguration( File filePrefix )
  {
    // schemas
    m_schemaURL = getClass().getResource( "schema/namodellV4.xsd" );
    m_controlSchemaURL = getClass().getResource( "schema/nacontrol.xsd" );
    //        m_schemaURL = getClass().getResource("schema/namodellV3.xsd");

    // formate:
    m_catchmentFormatURL = getClass().getResource( "formats/WernerCatchment.txt" );
    m_ChannelFormatURL = getClass().getResource( "formats/gerinne.txt" );

    m_netFormatURL = getClass().getResource( "formats/netzdatei.txt" );
    // ASCII
    (new File(filePrefix,"inp.dat")).mkdirs();
    m_catchmentFile = new File( filePrefix, "inp.dat/we_nat.geb" );
    m_channelFile = new File( filePrefix, "inp.dat/we_nat.ger" );
    m_netFile = new File( filePrefix, "inp.dat/we_nat.ntz" );

  }

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

  public URL getCatchmentFormatURL()
  {
    return m_catchmentFormatURL;
  }

  public File getCatchmentFile()
  {
    return m_catchmentFile;
  }

  public URL getNetFormatURL()
  {
    return m_netFormatURL;
  }

  public File getNetFile()
  {
    return m_netFile;
  }

  public URL getControlSchemaURL()
  {
    return m_controlSchemaURL;
  }
}