package org.kalypso.ui.wizards.imports.utils;

import java.awt.Color;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.Geometry_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * Utility class for creating SLD styles
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class StyleUtils
{
  private boolean m_DataPrepared = false;

  private boolean m_CreateFilter = false;

  private final String m_InputGML;

  private final String m_OutputSLD;

  private String m_GeometryProperty;

  private String m_FilterProperty;

  private Map<String, Color> m_customPropertyColorMap;

  private final String m_StyleLayerName;

  public StyleUtils( final String inputGML, final String outputSLD, final String geometryProperty, final String filterProperty, final HashMap<String, Color> customPropertyColorMap, final String styleLayerName )
  {
    super();
    m_InputGML = inputGML;
    m_OutputSLD = outputSLD;
    m_GeometryProperty = geometryProperty;
    setFilterProperty( filterProperty );
    m_customPropertyColorMap = customPropertyColorMap;
    m_StyleLayerName = styleLayerName;
  }

  public void setFilterProperty( final String filterProperty )
  {
    m_FilterProperty = filterProperty;
    m_CreateFilter = (m_FilterProperty != null && m_FilterProperty.trim().length() > 0);
  }

  public void importCustomFilterpropertyColorMap( final HashMap<String, Color> customPropertyColorMap, final boolean deleteExistingData )
  {
    if( m_customPropertyColorMap == null )
      m_customPropertyColorMap = new HashMap<>();
    if( deleteExistingData )
      m_customPropertyColorMap.clear();
    m_customPropertyColorMap.putAll( customPropertyColorMap );
  }

  public Map<String, Color> getFilterpropertyColorMap( ) throws Exception
  {
    prepareData();
    return m_customPropertyColorMap;
  }

  public void prepareData( ) throws Exception
  {
    if( m_DataPrepared )
      return;
    if( m_customPropertyColorMap == null )
      m_customPropertyColorMap = new HashMap<>();
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( new URL( "file:" + m_InputGML ), null ); //$NON-NLS-1$
    final String ns = workspace.getGMLSchema().getTargetNamespace();
    boolean b_geometryPropertyExists = (m_GeometryProperty != null && m_GeometryProperty.trim().length() > 0);
    final IFeatureType rootFeatureType = workspace.getRootFeature().getFeatureType();
    final IPropertyType[] propertyTypes = rootFeatureType.getProperties();
    try
    {
      for( final IPropertyType element : propertyTypes )
      {
        final Object object = workspace.getRootFeature().getProperty( element );
        if( object instanceof FeatureList )
        {
          final ListIterator<Feature> iterator = ((FeatureList) object).listIterator();
          while( iterator.hasNext() )
          {
            final Feature feature = iterator.next();
            try
            {
              if( !b_geometryPropertyExists )
              {
                m_GeometryProperty = feature.getFeatureType().getDefaultGeometryProperty().getQName().getLocalPart();
                b_geometryPropertyExists = true;
              }
              String styledOne = ""; //$NON-NLS-1$
              if( m_CreateFilter )
                styledOne = feature.getProperty( new QName( ns, m_FilterProperty ) ).toString();
              if( !m_customPropertyColorMap.containsKey( styledOne ) )
              {
                Color color = getRandomColor();
                while( m_customPropertyColorMap.containsValue( color ) )
                  color = getRandomColor();
                m_customPropertyColorMap.put( styledOne, color );
              }
            }
            catch( final Exception e )
            {
              System.out.println( e.getLocalizedMessage() );
              continue;
            }
          }

        }
      }
      m_DataPrepared = true;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public void createStyle( ) throws Exception
  {
    createStyle( m_OutputSLD );
  }

  public void createStyle( final String resultSLDFileName ) throws Exception
  {
    if( resultSLDFileName == null || resultSLDFileName.trim().length() == 0 )
      throw new Exception( "Result file name cannot be null or empty string." ); //$NON-NLS-1$
    prepareData();
    final double minScaleDenominator = 0;
    final double maxScaleDenominator = 1000000000000000.0;
    final Stroke stroke = StyleFactory.createStroke( new Color( 0, 0, 0 ), 1.0, 0.5, null, "mitre", "butt" ); //$NON-NLS-1$ //$NON-NLS-2$
    final FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle();
    Fill fill = null;
    final Iterator<Entry<String, Color>> iterator = m_customPropertyColorMap.entrySet().iterator();
    while( iterator.hasNext() )
    {
      final Map.Entry<String, Color> mapEntry = iterator.next();
      final Geometry geometry = new Geometry_Impl( new PropertyName( m_GeometryProperty, null ) );
      fill = StyleFactory.createFill( mapEntry.getValue(), 0.8 );
      final PolygonSymbolizer polygonSymbolizer = new PolygonSymbolizer_Impl( fill, stroke, geometry, UOM.pixel );
      final Symbolizer[] symbolizers = new Symbolizer[] { polygonSymbolizer };
      final Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator, maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if( m_CreateFilter )
      {
        final Operation operation = new PropertyIsLikeOperation( new PropertyName( m_FilterProperty, null ), new Literal( mapEntry.getKey() ), '*', '$', '/' );
        final Filter filter = new ComplexFilter( operation );
        rule.setFilter( filter );
      }
      featureTypeStyle.addRule( rule );
    }

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
    final Style[] styles = new Style[] { StyleFactory.createUserStyle( m_StyleLayerName, m_StyleLayerName, null, false, featureTypeStyles ) };
    final Layer[] layers = new Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
    final StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers ); //$NON-NLS-1$
    final Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    OutputStreamWriter os = null;
    try
    {
      os = new FileWriter( resultSLDFileName );
      final StreamResult result = new StreamResult( os );
      final TransformerFactory factory = TransformerFactory.newInstance();

      // Dejan: this works only with Java 1.5, in 1.4 it throws IllegalArgumentException
      // also, indentation doesn't works with OutputStream, only with OutputStreamWriter :)
      try
      {
        factory.setAttribute( "indent-number", new Integer( 4 ) ); //$NON-NLS-1$
      }
      catch( final IllegalArgumentException e )
      {
      }

      final Transformer transformer = factory.newTransformer();
      transformer.setOutputProperty( OutputKeys.ENCODING, "UTF-8" ); //$NON-NLS-1$
      // transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,"users.dtd"); //$NON-NLS-1$
      transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      // transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4"); //$NON-NLS-1$ //$NON-NLS-2$
      // transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "4"); //$NON-NLS-1$ //$NON-NLS-2$

      transformer.transform( source, result );
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  /**
   * Creates custom SLD file based on input GML file
   *
   * @param inputGMLFile
   *          - input GML file, full path
   * @param resultSLDFile
   *          - output SLD file, full path
   * @param styleLayerName
   *          - name for styled layer, eg. "Roughness style"
   * @param geometryProperty
   *          - optional; name of geometry property to use; if null or empty/whitespace string, default geometry
   *          property will be used instead
   * @param filterProperty
   *          - optional; name of the property on which values different styles will be defined; if null or
   *          empty/whitespace string, one default style will be created (no filtering)
   * @param knownPropertyColorSet
   *          - optional; set of known filterProperty values with preffered colors; doesn't have to be full set of all
   *          possible values; can also be null or empty; for values not contained in knownPropertyColorSet, (different)
   *          random colors will be used
   * @throws Exception
   *           - if GmlSerializer cannot create workspace based on input GML file
   * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
   */
  public static void createCustomStyle( final String inputGMLFile, final String resultSLDFile, final String styleLayerName, final String geometryProperty, final String filterProperty, final HashMap<String, Color> knownPropertyColorSet ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( new URL( "file:" + inputGMLFile ), null ); //$NON-NLS-1$
    final String ns = workspace.getGMLSchema().getTargetNamespace();
    String geometryPropertyName = geometryProperty;
    boolean b_geometryPropertyExists = (geometryPropertyName != null && geometryPropertyName.trim().length() > 0);
    final boolean b_filterPropertyExists = (filterProperty != null && filterProperty.trim().length() > 0);
    final boolean b_knownPropertyColorSetExists = (knownPropertyColorSet != null && knownPropertyColorSet.size() > 0);
    final IFeatureType rootFeatureType = workspace.getRootFeature().getFeatureType();
    final IPropertyType[] propertyTypes = rootFeatureType.getProperties();
    final HashMap<String, Color> filterPropertyColorMap = new HashMap<>();
    try
    {
      for( final IPropertyType element : propertyTypes )
      {
        final Object object = workspace.getRootFeature().getProperty( element );
        if( object instanceof FeatureList )
        {
          final ListIterator<Feature> iterator = ((FeatureList) object).listIterator();
          while( iterator.hasNext() )
          {
            final Feature feature = iterator.next();
            try
            {
              if( !b_geometryPropertyExists )
              {
                geometryPropertyName = feature.getFeatureType().getDefaultGeometryProperty().getQName().getLocalPart();
                b_geometryPropertyExists = true;
              }
              String styledOne = ""; //$NON-NLS-1$
              if( b_filterPropertyExists )
                styledOne = feature.getProperty( new QName( ns, filterProperty ) ).toString();
              if( !filterPropertyColorMap.containsKey( styledOne ) )
              {
                Color color = null;
                if( b_knownPropertyColorSetExists && knownPropertyColorSet.containsKey( styledOne ) )
                {
                  color = knownPropertyColorSet.get( styledOne );
                }
                else
                {
                  // don't want to put the same color twice...
                  color = getRandomColor();
                  while( filterPropertyColorMap.containsValue( color ) )
                    color = getRandomColor();
                }
                filterPropertyColorMap.put( styledOne, color );
              }
            }
            catch( final Exception e )
            {
              System.out.println( e.getLocalizedMessage() );
              continue;
            }
          }

        }
      }
      internal_createStyle( styleLayerName, geometryPropertyName, filterProperty, filterPropertyColorMap, resultSLDFile );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Helper function that is actually creating SLD file
   *
   * @param styleLayerName
   * @param geometryProperty
   * @param filterProperty
   * @param filterPropertyColorMap
   * @param resultSLDFile
   * @throws IOException
   *           - by FileWriter
   * @throws SAXException
   *           - by XML parser
   * @throws TransformerFactoryConfigurationError
   *           - by Transformer
   * @throws TransformerException
   *           - by Transformer
   */
  private static final void internal_createStyle( final String styleLayerName, final String geometryProperty, final String filterProperty, final Map<String, Color> filterPropertyColorMap, final String resultSLDFile ) throws IOException, SAXException, TransformerFactoryConfigurationError, TransformerException
  {
    final double minScaleDenominator = 0;
    final double maxScaleDenominator = 1000000000000000.0;
    final Stroke stroke = StyleFactory.createStroke( new Color( 0, 0, 0 ), 1.0, 0.5, null, "mitre", "butt" ); //$NON-NLS-1$ //$NON-NLS-2$
    final FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle();
    Fill fill = null;
    final Iterator<Entry<String, Color>> iterator = filterPropertyColorMap.entrySet().iterator();
    final boolean b_filterPropertySet = (filterProperty != null && filterProperty.trim().length() > 0);
    while( iterator.hasNext() )
    {
      final Map.Entry<String, Color> mapEntry = iterator.next();
      final Geometry geometry = new Geometry_Impl( new PropertyName( geometryProperty, null ) );
      fill = StyleFactory.createFill( mapEntry.getValue(), 0.8 );
      final PolygonSymbolizer polygonSymbolizer = new PolygonSymbolizer_Impl( fill, stroke, geometry, UOM.pixel );
      final Symbolizer[] symbolizers = new Symbolizer[] { polygonSymbolizer };
      final Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator, maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if( b_filterPropertySet )
      {
        final Operation operation = new PropertyIsLikeOperation( new PropertyName( filterProperty, null ), new Literal( mapEntry.getKey() ), '*', '$', '/' );
        final Filter filter = new ComplexFilter( operation );
        rule.setFilter( filter );
      }
      featureTypeStyle.addRule( rule );
    }

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
    final Style[] styles = new Style[] { StyleFactory.createUserStyle( styleLayerName, styleLayerName, null, false, featureTypeStyles ) };
    final Layer[] layers = new Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
    final StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers ); //$NON-NLS-1$
    final Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
    final Source source = new DOMSource( doc );
    OutputStreamWriter os = null;
    try
    {
      os = new FileWriter( resultSLDFile );
      final StreamResult result = new StreamResult( os );
      final TransformerFactory factory = TransformerFactory.newInstance();

      // Dejan: this works only with Java 1.5, in 1.4 it throws IllegalArgumentException
      // also, indentation doesn't works with OutputStream, only with OutputStreamWriter :)
      try
      {
        factory.setAttribute( "indent-number", new Integer( 4 ) ); //$NON-NLS-1$
      }
      catch( final IllegalArgumentException e )
      {
      }

      final Transformer transformer = factory.newTransformer();
      transformer.setOutputProperty( OutputKeys.ENCODING, "UTF-8" ); //$NON-NLS-1$
      // transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,"users.dtd"); //$NON-NLS-1$
      transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      // transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4"); //$NON-NLS-1$ //$NON-NLS-2$
      // transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "4"); //$NON-NLS-1$ //$NON-NLS-2$

      transformer.transform( source, result );
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  private static final Color getRandomColor( )
  {
    final int r = (int) (255.0 * Math.random());
    final int g = (int) (255.0 * Math.random());
    final int b = (int) (255.0 * Math.random());
    return new Color( r, g, b );
  }

}
