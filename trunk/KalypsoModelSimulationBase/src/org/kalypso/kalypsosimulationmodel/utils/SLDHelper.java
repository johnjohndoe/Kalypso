/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsosimulationmodel.utils;

import java.awt.Color;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.List;
import java.util.TreeMap;

import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Font;
import org.kalypsodeegree.graphics.sld.LabelPlacement;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PointPlacement;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.LabelPlacement_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointPlacement_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * TODO: move this into the deegree plug-in.
 *
 * @author Dejan Antanaskovic
 *
 */
public class SLDHelper
{
  // if this DEFAULT_STYLE_NAME and/or DEFAULT_STYLE_TITLE is changed,
  // it should be changed in all SLD layers in gmt files also
  public static final String DEFAULT_STYLE_NAME = "Kalypso style"; //$NON-NLS-1$

  public static final String DEFAULT_STYLE_TITLE = "Kalypso style"; //$NON-NLS-1$

  private static final String LAYER_NAME = "deegree style definition"; //$NON-NLS-1$

  private static final String LABEL_RULE_NAME = "Labelle"; //$NON-NLS-1$

  private static final double DEFAULT_VECTOR_FILLOPACITY = 0.75;

  private static final double DEFAULT_RASTER_FILLOPACITY = 0.9;

  private static final double DEFAULT_STROKEOPACITY = 1.0;

  private static final Color DEFAULT_STROKECOLOR = Color.BLACK;

  private static final double DEFAULT_STROKEWIDTH = 1.0;

  private static final String ELSEFILTER_NAME = "undefinierterStilID"; //$NON-NLS-1$

  private static final String ELSEFILTER_TITLE = Messages.getString( "org.kalypso.kalypsosimulationmodel.utils.SLDHelper.0" ); //$NON-NLS-1$

  private static final Color ELSEFILTER_FILLCOLOR = new Color( Integer.parseInt( "ffffff", 16 ) ); //$NON-NLS-1$

  private static final double ELSEFILTER_FILLOPACITY = 0.0;

  private static final Color ELSEFILTER_STROKECOLOR = new Color( Integer.parseInt( "ff0000", 16 ) ); //$NON-NLS-1$

  private static final double ELSEFILTER_STROKEOPACITY = 1.0;

  private static final double ELSEFILTER_STROKEWIDTH = 1.5;

  private static final float[] ELSEFILTER_DASHARRAY = new float[] { 2, 3.5f };

  public static void exportPolygonSymbolyzerSLD( final IFile sldFile, final Layer[] layers, final IProgressMonitor monitor ) throws IOException, SAXException, CoreException
  {
    final IProgressMonitor progressMonitor = monitor == null ? new NullProgressMonitor() : monitor;
    final StyledLayerDescriptor descriptor = createPolygonSLD( layers );
    exportSLD( sldFile, descriptor, progressMonitor );
  }

  public static void exportRasterSymbolyzerSLD( final IFile sldFile, final List< ? > collection, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws IOException, SAXException, CoreException
  {
    final IProgressMonitor progressMonitor = monitor == null ? new NullProgressMonitor() : monitor;
    final String myStyleName = styleName == null ? DEFAULT_STYLE_NAME : styleName;
    final String myStyleTitle = styleTitle == null ? DEFAULT_STYLE_TITLE : styleTitle;
    final StyledLayerDescriptor descriptor = createRasterSLD( collection, myStyleName, myStyleTitle, progressMonitor );
    exportSLD( sldFile, descriptor, progressMonitor );
  }

  public static void exportRasterSymbolyzerSLD( final IFile sldFile, final double minValue, final double maxValue, final int numberOfIntervals, final Color lightestColor, final Color darkestColor, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws IOException, SAXException, CoreException
  {
    final IProgressMonitor progressMonitor = monitor == null ? new NullProgressMonitor() : monitor;
    final String myStyleName = styleName == null ? DEFAULT_STYLE_NAME : styleName;
    final String myStyleTitle = styleTitle == null ? DEFAULT_STYLE_TITLE : styleTitle;
    final StyledLayerDescriptor descriptor = createRasterSLD( minValue, maxValue, numberOfIntervals, lightestColor, darkestColor, myStyleName, myStyleTitle, progressMonitor );
    exportSLD( sldFile, descriptor, progressMonitor );
  }

  private static void exportSLD( final IFile sldFile, final StyledLayerDescriptor descriptor, final IProgressMonitor progressMonitor ) throws IOException, SAXException, CoreException
  {
    // FIXME: ugly! The bug is elsewhere!
    if( !sldFile.isSynchronized( IResource.DEPTH_ZERO ) )
      sldFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );

    final ByteArrayInputStream stream = new ByteArrayInputStream( descriptor.exportAsXML().getBytes( "UTF-8" ) ); //$NON-NLS-1$
    final Document doc = XMLTools.parse( stream );
    final Source source = new DOMSource( doc );

    final SetContentHelper helper = new SetContentHelper()
    {
      @Override
      protected void write( final OutputStreamWriter writer ) throws Throwable
      {
        try
        {
          final StreamResult result = new StreamResult( writer );
          final TransformerFactory factory = TransformerFactory.newInstance();

          // Comment from Dejan: this works only with Java 1.5, in 1.4 it throws IllegalArgumentException
          // also, indentation doesn't works with OutputStream, only with OutputStreamWriter :)
          try
          {
            factory.setAttribute( "indent-number", new Integer( 4 ) ); //$NON-NLS-1$
          }
          catch( final IllegalArgumentException e )
          {
          }

          final Transformer transformer = factory.newTransformer();
          transformer.setOutputProperty( OutputKeys.ENCODING, writer.getEncoding() );
          transformer.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$

          // transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
          // //$NON-NLS-1$
          // //$NON-NLS-2$
          // transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
          // transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml"); //$NON-NLS-1$ //$NON-NLS-2$

          transformer.transform( source, result );
        }
        finally
        {
          IOUtils.closeQuietly( writer );
        }
      }
    };

    if( progressMonitor.isCanceled() )
      throw new CoreException( Status.CANCEL_STATUS );

    helper.setFileContents( sldFile, false, false, progressMonitor );
  }

  /**
   * if lightestColor is null, than WHITE (0, 0, 0) is used instead
   */
  private static StyledLayerDescriptor createRasterSLD( final double minValue, final double maxValue, final int numberOfIntervals, final Color lightestColor, final Color darkestColor, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws CoreException
  {
    final TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<>();
    final FeatureTypeStyle style = StyleFactory.createFeatureTypeStyle();

    final int rd = darkestColor.getRed();
    final int gd = darkestColor.getGreen();
    final int bd = darkestColor.getBlue();
    final int rl = lightestColor == null ? 0 : lightestColor.getRed();
    final int gl = lightestColor == null ? 0 : lightestColor.getGreen();
    final int bl = lightestColor == null ? 0 : lightestColor.getBlue();
    for( int i = 0; i <= numberOfIntervals; i++ )
    {
      final double ratio = (double) i / (double) numberOfIntervals;
      final double quantity = Math.rint( 1000.0 * (minValue + (maxValue - minValue) * ratio) ) / 1000.0;

      // making lighter color (color.brighter() is not so good...)
      final int r = (int) (rd * ratio + rl * (1 - ratio));
      final int g = (int) (gd * ratio + gl * (1 - ratio));
      final int b = (int) (bd * ratio + bl * (1 - ratio));

      final ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( new Color( r, g, b ), DEFAULT_RASTER_FILLOPACITY, quantity, "" ); //$NON-NLS-1$
      colorMap.put( quantity, colorMapEntry );
      if( monitor.isCanceled() )
        throw new CoreException( Status.CANCEL_STATUS );
    }

    final RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( null, colorMap, null, null );
    final Rule rule = StyleFactory.createRule( rasterSymbolizer );
    style.addRule( rule );

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final Style[] styles = new Style[] { StyleFactory.createUserStyle( styleName, styleTitle, null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( LAYER_NAME, null, styles ) };
    return SLDFactory.createStyledLayerDescriptor( layers );
  }

  private static StyledLayerDescriptor createRasterSLD( final List< ? > collection, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws CoreException
  {
    final TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<>();
    final FeatureTypeStyle style = StyleFactory.createFeatureTypeStyle();

    //    colorMap.put( -Double.MAX_VALUE, new ColorMapEntry_Impl( Color.WHITE, DEFAULT_RASTER_FILLOPACITY, -Double.MAX_VALUE, Messages.getString( "org.kalypso.kalypsosimulationmodel.utils.SLDHelper.1" ) ) ); //$NON-NLS-1$
    for( final Object styledFeatureObject : collection )
    {
      if( monitor.isCanceled() )
        throw new CoreException( Status.CANCEL_STATUS );
      if( styledFeatureObject instanceof IColorStyledFeatureWrapper )
      {
        final IColorStyledFeatureWrapper styledFeature = (IColorStyledFeatureWrapper) styledFeatureObject;

        final RGB rgb = styledFeature.getColorStyle();
        final Color color = rgb == null ? Color.WHITE : new Color( rgb.red, rgb.green, rgb.blue );
        final double quantity = styledFeature.getOrdinalNumber();
        final String featureName = styledFeature.getName();
        final String label = featureName == null || featureName.length() == 0 ? Messages.getString( "org.kalypso.kalypsosimulationmodel.utils.SLDHelper.2" ) : featureName; //$NON-NLS-1$
        final ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, DEFAULT_RASTER_FILLOPACITY, quantity, label );
        colorMap.put( new Double( quantity ), colorMapEntry );
      }
      else if( styledFeatureObject instanceof ColorMapEntry )
      {
        final ColorMapEntry entry = (ColorMapEntry) styledFeatureObject;
        entry.setOpacity( DEFAULT_RASTER_FILLOPACITY );
        colorMap.put( entry.getQuantity(), entry );
      }
    }

    final RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( null, colorMap, null, null );
    final Rule rule = StyleFactory.createRule( rasterSymbolizer );
    style.addRule( rule );

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final Style[] styles = new Style[] { StyleFactory.createUserStyle( styleName, styleTitle, null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( LAYER_NAME, null, styles ) };
    return SLDFactory.createStyledLayerDescriptor( layers );
  }

  public static Layer polygonStyleLayer( final String layerName, final List< ? > collection, final QName geometryProperty, final QName styleProperty, final String styleName, final String styleTitle, final IProgressMonitor progressMonitor ) throws CoreException
  {
    final String myLayerName = layerName == null ? LAYER_NAME : layerName;
    final FeatureTypeStyle style = StyleFactory.createFeatureTypeStyle();
    final PropertyName geomPropertyName = geometryProperty == null ? null : new PropertyName( geometryProperty );
    final String myStyleName = styleName == null ? DEFAULT_STYLE_NAME : styleName;
    final String myStyleTitle = styleTitle == null ? DEFAULT_STYLE_TITLE : styleTitle;

    // creating the ElseFilter rule
    final Stroke defaultRuleStroke = StyleFactory.createStroke( ELSEFILTER_STROKECOLOR, ELSEFILTER_STROKEWIDTH, ELSEFILTER_STROKEOPACITY );
    defaultRuleStroke.setDashArray( ELSEFILTER_DASHARRAY );
    final Fill defaultRuleFill = StyleFactory.createFill( ELSEFILTER_FILLCOLOR, ELSEFILTER_FILLOPACITY );
    final PolygonSymbolizer defaultRuleSymbolizer = StyleFactory.createPolygonSymbolizer( defaultRuleStroke, defaultRuleFill, geomPropertyName );
    final Rule defaultRule = StyleFactory.createRule( defaultRuleSymbolizer );
    defaultRule.setElseFilter( true );
    defaultRule.setName( ELSEFILTER_NAME );
    defaultRule.setTitle( ELSEFILTER_TITLE );
    defaultRule.setAbstract( ELSEFILTER_TITLE );
    style.addRule( defaultRule );

    // adding rules for every member
    for( final Object styledFeatureObject : collection )
    {
      final IColorStyledFeatureWrapper styledFeature;
      if( styledFeatureObject instanceof IColorStyledFeatureWrapper )
        styledFeature = (IColorStyledFeatureWrapper) styledFeatureObject;
      else
        continue;
      if( progressMonitor.isCanceled() )
        throw new CoreException( Status.CANCEL_STATUS );

      final RGB rgb = styledFeature.getColorStyle();
      Color color = null;
      if( rgb == null )
        color = Color.WHITE;
      else
        color = new Color( rgb.red, rgb.green, rgb.blue );
      final Stroke stroke = StyleFactory.createStroke( DEFAULT_STROKECOLOR, DEFAULT_STROKEWIDTH, DEFAULT_STROKEOPACITY );
      final Fill fill = StyleFactory.createFill( color, DEFAULT_VECTOR_FILLOPACITY );
      final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, geomPropertyName );
      final Rule rule = StyleFactory.createRule( newSymbolizer );
      final String ruleName = styledFeature.getName();
      if( ruleName == null )
        continue;

      final Operation operation = new PropertyIsLikeOperation( new PropertyName( styleProperty.getLocalPart(), null ), new Literal( ruleName ), '*', '$', '/' );
      final Filter filter = new ComplexFilter( operation );
      rule.setName( styledFeature.getId() );
      rule.setTitle( ruleName );
      rule.setAbstract( ruleName );
      rule.setFilter( filter );
      style.addRule( rule );
    }

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final UserStyle userStyle = StyleFactory.createUserStyle( myStyleName, myStyleTitle, null, false, featureTypeStyles );
    final Style[] styles = new Style[] { userStyle };
    return SLDFactory.createNamedLayer( myLayerName, null, styles );
  }

  public static Layer textlabelStyleLayer( final String layerName, final QName geometryProperty, final QName styleProperty, final String styleName, final String styleTitle )
  {
    final String myLayerName = layerName == null ? LAYER_NAME : layerName;
    final FeatureTypeStyle style = StyleFactory.createFeatureTypeStyle();
    final PropertyName geomPropertyName = geometryProperty == null ? null : new PropertyName( geometryProperty );
    final String myStyleName = styleName == null ? DEFAULT_STYLE_NAME : styleName;
    final String myStyleTitle = styleTitle == null ? DEFAULT_STYLE_TITLE : styleTitle;

    final ParameterValueType[] anchorPoint = new ParameterValueType[2];
    anchorPoint[0] = StyleFactory.createParameterValueType( 0.5 );
    anchorPoint[1] = StyleFactory.createParameterValueType( 0.5 );
    final ParameterValueType rotation = StyleFactory.createParameterValueType( 0.0 );

    // adding labels rule
    final Font font = StyleFactory.createFont( "Arial", 11.0 ); //$NON-NLS-1$
    font.setColor( Color.BLACK );
    final PointPlacement pointPlacement = new PointPlacement_Impl( anchorPoint, new ParameterValueType[0], rotation, true );
    final LabelPlacement labelPlacement = new LabelPlacement_Impl( pointPlacement );
    final TextSymbolizer labelSymbolizer = StyleFactory.createTextSymbolizer( geomPropertyName, "<ogc:PropertyName>" + styleProperty.getLocalPart() + "</ogc:PropertyName>", labelPlacement ); //$NON-NLS-1$ //$NON-NLS-2$
    labelSymbolizer.setHalo( null );
    labelSymbolizer.setFont( font );
    final Rule labelRule = StyleFactory.createRule( labelSymbolizer, 0.0, 250.0 );
    labelRule.setName( LABEL_RULE_NAME );
    labelRule.setTitle( LABEL_RULE_NAME );
    labelRule.setAbstract( LABEL_RULE_NAME );
    style.addRule( labelRule );

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final UserStyle userStyle = StyleFactory.createUserStyle( myStyleName, myStyleTitle, null, false, featureTypeStyles );
    final Style[] styles = new Style[] { userStyle };
    return SLDFactory.createNamedLayer( myLayerName, null, styles );
  }

  private static StyledLayerDescriptor createPolygonSLD( final Layer[] layers )
  {
    return SLDFactory.createStyledLayerDescriptor( layers );
  }
}
