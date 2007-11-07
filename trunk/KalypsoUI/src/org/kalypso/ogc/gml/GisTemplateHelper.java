/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.Writer;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.io.ReaderUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;
import org.kalypso.ogc.gml.wms.provider.WMSImageProvider;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.gismapview.CascadingLayer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistreeview.Gistreeview;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * Hilfsklasse, um aus den Binding-Klassen 'echte' Objekte zu erzeugen und umgekehrt
 * 
 * @author Belger
 */
public class GisTemplateHelper
{
  public static final ObjectFactory OF_GISMAPVIEW = new ObjectFactory();

  public static final org.kalypso.template.types.ObjectFactory OF_TEMPLATE_TYPES = new org.kalypso.template.types.ObjectFactory();

  private GisTemplateHelper( )
  {
    // never instantiate this class
  }

  public static final Featuretemplate loadGisFeatureTemplate( final IFile file, final Properties replaceProps ) throws CoreException, IOException, JAXBException
  {
    // TODO: replace with 'ReplaceToken'
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file.getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );
    return GisTemplateHelper.loadGisFeatureTemplate( new InputSource( new StringReader( contents ) ) );
  }

  public static final Featuretemplate loadGisFeatureTemplate( final InputSource is ) throws JAXBException
  {
    final JAXBContext context = JaxbUtilities.createQuiet( org.kalypso.template.featureview.ObjectFactory.class );
    final Unmarshaller unmarshaller = context.createUnmarshaller();
    return (Featuretemplate) unmarshaller.unmarshal( is );
  }

  public static final Gismapview loadGisMapView( final IFile file, final Properties replaceProps ) throws CoreException, IOException, JAXBException
  {
    // TODO: replace with 'ReplaceToken'
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file.getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );
    return GisTemplateHelper.loadGisMapView( new InputSource( new StringReader( contents ) ) );
  }

  public static final Gismapview loadGisMapView( final IStorage file ) throws JAXBException, CoreException
  {
    final InputSource is = new InputSource( file.getContents() );
    if( file instanceof IEncodedStorage )
      is.setEncoding( ((IEncodedStorage) file).getCharset() );
    return GisTemplateHelper.loadGisMapView( is );
  }

  public static final Gismapview loadGisMapView( final File file ) throws IOException, JAXBException
  {
    BufferedInputStream inputStream = null;
    try
    {
      inputStream = new BufferedInputStream( new FileInputStream( file ) );
      final Gismapview gisMapView = GisTemplateHelper.loadGisMapView( new InputSource( inputStream ) );
      inputStream.close();
      return gisMapView;
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }

  }

  // TODO: for all calling methods: close streams!
  public static final Gismapview loadGisMapView( final InputSource is ) throws JAXBException
  {
    final Unmarshaller unmarshaller = TemplateUtilitites.createGismapviewUnmarshaller();

    try
    {
      // XInclude awareness
      final SAXParserFactory spf = SAXParserFactory.newInstance();
      spf.setNamespaceAware( true );
      spf.setXIncludeAware( true );
      final XMLReader xr = spf.newSAXParser().getXMLReader();
      xr.setContentHandler( unmarshaller.getUnmarshallerHandler() );
      xr.parse( is );
      return (Gismapview) unmarshaller.getUnmarshallerHandler().getResult();
    }
    // TODO: throw all these exceptions!
    catch( final SAXException e )
    {
      e.printStackTrace();
    }
    catch( final ParserConfigurationException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * F�hrt ein Pattern-Ersetzen durch, bevor die Gistableview geparst wird Jeder key der Properties wird durch seinen
   * value ersetzt. Funktioniert nur zeilenweise, d.h.
   * 
   * @param file
   * @param replaceProps
   * @return Gistableview
   * @throws CoreException
   * @throws CoreException
   * @throws IOException
   * @throws JAXBException
   */
  public static Gistableview loadGisTableview( final IFile file, final Properties replaceProps ) throws CoreException, IOException, JAXBException
  {
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file.getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );
    return GisTemplateHelper.loadGisTableview( new InputSource( new StringReader( contents ) ) );
  }

  public static Gistableview loadGisTableview( final IFile file ) throws CoreException, JAXBException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );
    return GisTemplateHelper.loadGisTableview( is );
  }

  public static Gistableview loadGisTableview( final InputSource is ) throws JAXBException
  {
    final Unmarshaller unmarshaller = TemplateUtilitites.JC_GISTABLEVIEW.createUnmarshaller();
    return (Gistableview) unmarshaller.unmarshal( is );
  }

  public static void saveGisMapView( final Gismapview modellTemplate, final OutputStream outStream, final String encoding ) throws JAXBException
  {
    final Marshaller marshaller = TemplateUtilitites.createGismapviewMarshaller( encoding );
    marshaller.marshal( modellTemplate, outStream );
  }

  public static void saveGisMapView( final Gismapview modellTemplate, final Writer writer, final String encoding ) throws JAXBException
  {
    final Marshaller marshaller = TemplateUtilitites.createGismapviewMarshaller( encoding );
    marshaller.marshal( modellTemplate, writer );
  }

  public static GM_Envelope getBoundingBox( final Gismapview gisview )
  {
    final ExtentType extent = gisview.getExtent();
    if( extent == null )
      return null;

    final GM_Envelope env = GeometryFactory.createGM_Envelope( extent.getLeft(), extent.getBottom(), extent.getRight(), extent.getTop() );
    final String orgSRSName = extent.getSrs();
    if( orgSRSName != null )
      try
      {
        final CS_CoordinateSystem targetSRS = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
        if( (orgSRSName != null) && !orgSRSName.equals( targetSRS.getName() ) )
        {
          // if srs attribute exists and it is not the target srs we have to convert it
          final GeoTransformer transformer = new GeoTransformer( targetSRS );
          return transformer.transformEnvelope( env, orgSRSName );
        }
      }
      catch( final Exception e )
      {
        // we just print the error, but asume that we can return an envelope that is not converted
        e.printStackTrace();
      }
    return env;
  }

  /**
   * This method creates a new Map with a bounding box
   * 
   * @return gismapview new empty map with a layer list
   */
  public static Gismapview emptyGisView( )
  {
    final GM_Envelope dummyBBox = GeometryFactory.createGM_Envelope( 0, 0, 100, 100 );
    final Gismapview gismapview = GisTemplateHelper.OF_GISMAPVIEW.createGismapview();
    final Layers layersType = GisTemplateHelper.OF_GISMAPVIEW.createGismapviewLayers();
    layersType.setActive( null );
    if( dummyBBox != null )
    {
      final ExtentType extentType = GisTemplateHelper.OF_TEMPLATE_TYPES.createExtentType();
      extentType.setTop( dummyBBox.getMax().getY() );
      extentType.setBottom( dummyBBox.getMin().getY() );
      extentType.setLeft( dummyBBox.getMin().getX() );
      extentType.setRight( dummyBBox.getMax().getX() );
      gismapview.setExtent( extentType );
    }
    gismapview.setLayers( layersType );
    return gismapview;
  }

  public static GM_Surface< ? > getBoxAsSurface( final Gismapview mapview, final CoordinateSystem targetCS ) throws Exception
  {
    final ExtentType extent = mapview.getExtent();
    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final String crsName = extent.getSrs();
    if( csFac.isKnownCS( crsName ) )
    {
      final GM_Envelope envelope = GeometryFactory.createGM_Envelope( extent.getLeft(), extent.getBottom(), extent.getRight(), extent.getTop() );
      final CS_CoordinateSystem crs = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( csFac.getCSByName( crsName ) );
      final GM_Surface< ? > bboxAsSurface = GeometryFactory.createGM_Surface( envelope, crs );
      if( targetCS != null )
      {
        final GeoTransformer transformer = new GeoTransformer( targetCS );
        return (GM_Surface< ? >) transformer.transform( bboxAsSurface );
      }
      return bboxAsSurface;
    }
    return null;
  }

  /**
   * @param strictType
   *            If true, use the real FeatureType of the first feature of the given collection to strictly type the
   *            layer.
   */
  public static Gismapview createGisMapView( final Map<Feature, IRelationType> layersToCreate, final boolean strictType )
  {
    final Gismapview gismapview = TemplateUtilitites.OF_GISMAPVIEW.createGismapview();
    final Layers layers = TemplateUtilitites.OF_GISMAPVIEW.createGismapviewLayers();
    gismapview.setLayers( layers );

    final List<JAXBElement< ? extends StyledLayerType>> layer = layers.getLayer();

    int count = 0;
    for( final Map.Entry<Feature, IRelationType> entry : layersToCreate.entrySet() )
    {
      final Feature feature = entry.getKey();
      final URL context = feature.getWorkspace().getContext();

      final IRelationType rt = entry.getValue();

      final String parentName = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_NAME );
      final String parentLabel = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
      final String layerName = parentName + " - " + parentLabel;

      final FeaturePath featurePathToParent = new FeaturePath( feature );

      final Object property = feature.getProperty( rt );
      String typeName = "";
      if( strictType && (property instanceof List) )
      {
        final List< ? > list = (List< ? >) property;
        if( !list.isEmpty() )
        {
          final Feature firstChild = FeatureHelper.getFeature( feature.getWorkspace(), list.get( 0 ) );
          typeName = "[" + firstChild.getFeatureType().getQName().getLocalPart() + "]";
        }
      }

      final String memberName = rt.getQName().getLocalPart() + typeName;

      final FeaturePath featurePath = new FeaturePath( featurePathToParent, memberName );

      final StyledLayerType layerType = TemplateUtilitites.OF_TYPES.createStyledLayerType();
      layerType.setHref( context.toExternalForm() );
      layerType.setFeaturePath( featurePath.toString() );
      layerType.setLinktype( "gml" );
      layerType.setName( layerName );
      layerType.setVisible( true );
      layerType.setId( "ID_" + count++ );

      final JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( layerType );

      layer.add( layerElement );
    }

    if( layer.size() > 0 )
      layers.setActive( layer.get( 0 ).getValue() );

    return gismapview;
  }

  public static final Gistreeview loadGisTreeView( final IStorage file ) throws JAXBException, CoreException
  {
    final InputSource is = new InputSource( file.getContents() );
    if( file instanceof IEncodedStorage )
      is.setEncoding( ((IEncodedStorage) file).getCharset() );
    return GisTemplateHelper.loadGisTreeView( is );
  }

  public static final Gistreeview loadGisTreeView( final InputSource is ) throws JAXBException
  {
    final Unmarshaller unmarshaller = TemplateUtilitites.JC_GISTREEVIEW.createUnmarshaller();
    return (Gistreeview) unmarshaller.unmarshal( is );
  }

  public static void saveGisMapView( final Gismapview gisMapView, final File outFile, final String encoding ) throws IOException, JAXBException
  {
    BufferedOutputStream os = null;
    try
    {
      os = new BufferedOutputStream( new FileOutputStream( outFile ) );
      GisTemplateHelper.saveGisMapView( gisMapView, os, encoding );
      os.close();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  public static StyledLayerType addLayer( final List<JAXBElement< ? extends StyledLayerType>> layerList, final IKalypsoTheme theme, final int count, final GM_Envelope bbox, final String srsName, final IProgressMonitor monitor ) throws CoreException
  {
    final org.kalypso.template.gismapview.ObjectFactory maptemplateFactory = new org.kalypso.template.gismapview.ObjectFactory();
    final org.kalypso.template.types.ObjectFactory templateFactory = new org.kalypso.template.types.ObjectFactory();

    final StyledLayerType layer = templateFactory.createStyledLayerType();

    if( theme instanceof GisTemplateFeatureTheme )
    {
      ((GisTemplateFeatureTheme) theme).fillLayerType( layer, "ID_" + count, theme.isVisible() );//$NON-NLS-1$
      final JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( layer );
      layerList.add( layerElement );
      monitor.worked( 1000 );

      return layer;
    }
    else if( theme instanceof KalypsoWMSTheme )
    {
      final String name = theme.getName();

      layer.setName( name );
      layer.setFeaturePath( "" ); //$NON-NLS-1$
      layer.setVisible( theme.isVisible() );
      layer.setId( "ID_" + count );
      layer.setHref( ((KalypsoWMSTheme) theme).getSource() );
      layer.setLinktype( WMSImageProvider.TYPE_NAME );
      layer.setActuate( "onRequest" ); //$NON-NLS-1$
      layer.setType( "simple" ); //$NON-NLS-1$

      final JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( layer );

      layerList.add( layerElement );
      monitor.worked( 1000 );

      return layer;
    }
    else if( theme instanceof KalypsoPictureTheme )
    {
      ((KalypsoPictureTheme) theme).fillLayerType( layer, "ID_" + count, theme.isVisible() ); //$NON-NLS-1$

      final JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( layer );

      layerList.add( layerElement );
      monitor.worked( 1000 );
    }
    else if( theme instanceof CascadingKalypsoTheme )
    {
      final CascadingKalypsoTheme cascadingKalypsoTheme = ((CascadingKalypsoTheme) theme);
      cascadingKalypsoTheme.fillLayerType( layer, "ID_" + count, theme.isVisible() ); //$NON-NLS-1$

      final JAXBElement<StyledLayerType> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createLayer( layer );

      layerList.add( layerElement );

      cascadingKalypsoTheme.createGismapTemplate( bbox, srsName, new SubProgressMonitor( monitor, 1000 ) );

      return layer;
    }
    else if( theme instanceof CascadingLayerKalypsoTheme )
    {
      final CascadingLayer cascadingLayer = maptemplateFactory.createCascadingLayer();

      final CascadingLayerKalypsoTheme cascadingKalypsoTheme = ((CascadingLayerKalypsoTheme) theme);
      cascadingKalypsoTheme.fillLayerType( cascadingLayer, "ID_" + count, theme.isVisible(), srsName, monitor ); //$NON-NLS-1$

      final JAXBElement<CascadingLayer> layerElement = TemplateUtilitites.OF_GISMAPVIEW.createCascadingLayer( cascadingLayer );
      layerList.add( layerElement );

      return cascadingLayer;
    }
    else if( theme instanceof KalypsoLegendTheme )
    {
      final String name = theme.getName();

      layer.setName( name );
      layer.setFeaturePath( "" ); //$NON-NLS-1$
      layer.setVisible( theme.isVisible() );
      layer.setId( "ID_" + count );
      layer.setHref( "" );

      layer.setLinktype( "legend" );

      layerList.add( TemplateUtilitites.OF_GISMAPVIEW.createLayer( layer ) );

      monitor.worked( 1000 );

      return layer;
    }
    else if( theme instanceof ScrabLayerFeatureTheme )
    {
      final String name = theme.getName();

      layer.setName( name );
      layer.setFeaturePath( "" ); //$NON-NLS-1$
      layer.setVisible( theme.isVisible() );
      layer.setId( "ID_" + count );
      layer.setHref( "" );

      layer.setLinktype( "scrab" );

      layerList.add( TemplateUtilitites.OF_GISMAPVIEW.createLayer( layer ) );

      monitor.worked( 1000 );

      return layer;
    }

    return null;
  }

}