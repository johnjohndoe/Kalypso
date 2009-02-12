package org.kalypso.wss.provider;

import java.awt.Font;
import java.awt.Image;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.ogc.gml.map.themes.KalypsoRemoteWMService;
import org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider;
import org.kalypso.ogc.gml.map.themes.provider.IKalypsoLegendProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This provider loads an image from a WSS/WMS. It caches the capabilities, so that this request is only done once.
 * 
 * @author Holger Albert
 */
public abstract class KalypsoWSSImageProvider implements IKalypsoImageProvider, IKalypsoLegendProvider
{
  /**
   * This variable stores the name of the theme.
   */
  private String m_themeName;

  /**
   * This variable stores some parameters for the WMS.
   */
  private String m_source;

  /**
   * This variable stores the client coordinate system.
   */
  private CS_CoordinateSystem m_localSRS;

  /**
   * This variable stores the WMS service or is null.
   */
  private KalypsoRemoteWMService m_wms;

  /**
   * The constructor.
   */
  public KalypsoWSSImageProvider( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#init(java.lang.String, java.lang.String,
   *      org.opengis.cs.CS_CoordinateSystem)
   */
  public void init( String themeName, String source, CS_CoordinateSystem localSRS )
  {
    m_themeName = themeName;
    m_source = source;
    m_localSRS = localSRS;

    m_wms = null;

    /* Will inititialize the credentials and other things for the WSS. */
    initWSS();
  }

  /**
   * This function initializes the WSS specific stuff.
   */
  protected abstract void initWSS( );

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getImage(int, int,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public Image getImage( int width, int height, GM_Envelope bbox ) throws CoreException
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getLabel()
   */
  public String getLabel( )
  {
    return "WSS Thema: " + m_source;
  }

  /**
   * This function returns the href of the request.
   * 
   * @return The href of the request.
   */
  public String getSource( )
  {
    return m_source;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getFullExtent()
   */
  public GM_Envelope getFullExtent( )
  {
    GM_Envelope maxExtent = null;

    try
    {
      maxExtent = m_wms.getMaxExtend( m_localSRS );
    }
    catch( CoreException e )
    {
      /* Do nothing. */
    }

    return maxExtent;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoLegendProvider#getLegendGraphic(java.awt.Font,
   *      java.lang.String)
   */
  public Image getLegendGraphic( Font font, String layerName ) throws CoreException
  {
    return null;
  }
}