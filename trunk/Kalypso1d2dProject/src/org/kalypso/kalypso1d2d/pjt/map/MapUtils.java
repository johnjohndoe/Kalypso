package org.kalypso.kalypso1d2d.pjt.map;

import java.awt.Color;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.core.jaxb.TemplateUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.wizards.imports.utils.StyleUtils;
import org.kalypso.ui.wizards.results.IResultThemeConstructor;
import org.kalypso.ui.wizards.results.IThemeConstructionFactory;
import org.kalypso.ui.wizards.results.ResultAddLayerCommandData;

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
  public MapUtils( final String gmtFileName, final String gmlFileName, final String sldFileName, final String gmlFileRelativePath, final String featurePath, final ExtentType extentType, final String layerName, final String styleLayerName, final String geometryPropertyName, final String filterPropertyName, final HashMap<String, Color> customValueColorMap )
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
  public void createMap( final boolean createStyle ) throws IOException, JAXBException
  {
    final FileWriter writer = new FileWriter( m_GmtFileName );
    final Gismapview gismapview = GisTemplateHelper.emptyGisView();
    gismapview.setExtent( m_ExtentType );
    final Layers layers = gismapview.getLayers();
    final StyledLayerType element = new StyledLayerType();
    element.setId( "layer_1" ); //$NON-NLS-1$
    element.setLinktype( "gml" ); //$NON-NLS-1$
    element.setType( "simple" ); //$NON-NLS-1$
    element.setName( m_LayerName );
    element.setActuate( "onRequest" ); //$NON-NLS-1$
    element.setFeaturePath( m_FeaturePath );
    element.setHref( m_GmlFileRelativePath );
    element.setVisible( true );

    final List<Style> styleList = element.getStyle();
    final Style style = new Style();

    // set attributes for the style
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( m_StyleLayerName );
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( FileUtilities.nameFromPath( m_SldFileName ) );
    style.setType( "simple" ); //$NON-NLS-1$
    styleList.add( style );

    final JAXBElement<StyledLayerType> layerType = TemplateUtilities.OF_GISMAPVIEW.createLayer( element );

    layers.getLayer().add( 0, layerType );
    GisTemplateHelper.saveGisMapView( gismapview, writer, "UTF8" ); //$NON-NLS-1$
    writer.close();
    if( createStyle )
      try
      {
        final StyleUtils styleUtils = new StyleUtils( m_GmlFileName, m_SldFileName, m_GeometryPropertyName, m_FilterPropertyName, m_CustomValueColorMap, m_StyleLayerName );
        styleUtils.createStyle();
        // StyleUtils.createCustomStyle( m_GmlFileName, m_SldFileName, m_StyleLayerName, m_GeometryPropertyName,
        // m_FilterPropertyName, m_CustomValueColorMap );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
  }

  /**
   * TODO: maybe move into helper class
   */
  public static IStatus addThemes( final IKalypsoLayerModell modell, final ICommandTarget commandTarget, final IResultMeta[] results, final IThemeConstructionFactory factory, final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.MapUtils.5"), results.length ); //$NON-NLS-1$

    for( final IResultMeta resultMeta : results )
    {
      final IResultThemeConstructor themeCreator = factory.createThemeConstructor( resultMeta );
      final ResultAddLayerCommandData[] datas = themeCreator.getThemeCommandData();
      if( datas != null )
      {
        for( final ResultAddLayerCommandData data : datas )
        {
          if( modell != null )
          {
            final AddThemeCommand addThemeCommand = new AddThemeCommand( modell, data.getThemeName(), data.getResultType(), data.getFeaturePath(), data.getSource() );
            addThemeCommand.addStyle( data.getStyle(), data.getStyleLocation() );
            addThemeCommand.addProperties( data.getProperties() );
            commandTarget.postCommand( addThemeCommand, null );
          }
        }
      }

      // TODO:
      // - create sub-themes for container results (also use filter for children)
      // - ...

      monitor.worked( 1 );
      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;
    }
    return Status.OK_STATUS;

  }

}
