package org.kalypso.ui.shapeImportWizards.utils;

import java.awt.Color;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;

/**
 * Utility class for creating GisMapView map files
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class MapUtils
{

  public String m_GmtFileName;

  public String m_GmlFileName;

  public String m_SldFileName;

  public String m_GmlFileRelativePath;

  public String m_FeaturePath;

  public ExtentType m_ExtentType;

  public String m_LayerName;

  public String m_StyleLayerName;

  public String m_GeometryPropertyName;

  public String m_FilterPropertyName;

  public HashMap<String, Color> m_CustomValueColorMap;

  public MapUtils( )
  {
    super();
  }

  /**
   * @param gmtFileName
   * @param gmlFileName
   * @param sldFileName
   * @param gmlFileRelativePath
   * @param featurePath
   * @param extentType
   * @param layerName
   * @param styleLayerName
   * @param geometryPropertyName
   * @param filterPropertyName
   * @param customValueColorMap
   */
  public MapUtils( String gmtFileName, String gmlFileName, String sldFileName, String gmlFileRelativePath, String featurePath, ExtentType extentType, String layerName, String styleLayerName, String geometryPropertyName, String filterPropertyName, HashMap<String, Color> customValueColorMap )
  {
    super();
    m_GmtFileName = gmtFileName;
    m_GmlFileName = gmlFileName;
    m_SldFileName = sldFileName;
    m_GmlFileRelativePath = gmlFileRelativePath;
    m_FeaturePath = featurePath;
    m_ExtentType = extentType;
    m_LayerName = layerName;
    m_StyleLayerName = styleLayerName;
    m_GeometryPropertyName = geometryPropertyName;
    m_FilterPropertyName = filterPropertyName;
    m_CustomValueColorMap = customValueColorMap;
  }

  /**
   * @throws IOException
   * @throws JAXBException
   */
  public void createMap( boolean createStyle ) throws IOException, JAXBException
  {
    FileWriter writer = new FileWriter( m_GmtFileName );
    final Gismapview gismapview = GisTemplateHelper.emptyGisView();
    gismapview.setExtent( m_ExtentType );
    Layers layers = gismapview.getLayers();
    StyledLayerType element = new StyledLayerType();
    element.setId( "layer_1" );
    element.setLinktype( "gml" );
    element.setType( "simple" );
    element.setName( m_LayerName );
    element.setActuate( "onRequest" );
    element.setFeaturePath( m_FeaturePath );
    element.setHref( m_GmlFileRelativePath );
    element.setVisible( true );

    final List<Style> styleList = element.getStyle();
    final Style style = new Style();

    // set attributes for the style
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( m_StyleLayerName ); //$NON-NLS-1$
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( FileUtilities.nameFromPath( m_SldFileName ) ); //$NON-NLS-1$
    style.setType( "simple" ); //$NON-NLS-1$    
    styleList.add( style );

    layers.getLayer().add( 0, element );
    GisTemplateHelper.saveGisMapView( gismapview, writer, "UTF8" );
    writer.close();
    if( createStyle )
    {
      try
      {
        StyleUtils styleUtils = new StyleUtils( m_GmlFileName, m_SldFileName, m_GeometryPropertyName, m_FilterPropertyName, m_CustomValueColorMap, m_StyleLayerName );
        styleUtils.createStyle();
        // StyleUtils.createCustomStyle( m_GmlFileName, m_SldFileName, m_StyleLayerName, m_GeometryPropertyName,
        // m_FilterPropertyName, m_CustomValueColorMap );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }
}
