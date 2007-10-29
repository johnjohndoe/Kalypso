/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Font;
import org.kalypsodeegree.graphics.sld.LabelPlacement;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PointPlacement;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.LabelPlacement_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointPlacement_Impl;
import org.kalypsodeegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class SLDHelper
{
  private static final String LAYER_NAME = "deegree style definition";

  private static final String LABEL_RULE_NAME = "Labelle";

  private static final String DEFAULT_RULE_NAME = "undefinierterStilID";

  private static final String DEFAULT_RULE_TITLE = "undefinierter Stil";

  private static final Color DEFAULT_RULE_FILLCOLOR = new Color( Integer.parseInt( "ffffff", 16 ) ); //$NON-NLS-1$

  private static final double DEFAULT_RULE_FILLOPACITY = 0.0;

  private static final Color DEFAULT_RULE_STROKECOLOR = new Color( Integer.parseInt( "ff0000", 16 ) ); //$NON-NLS-1$

  private static final double DEFAULT_RULE_STROKEOPACITY = 1.0;

  private static final double DEFAULT_RULE_STROKEWIDTH = 2.0;

  private static final float[] DEFAULT_RULE_DASHARRAY = new float[] { 2, 5 };

  public static void exportPolygonSymbolyzerSLD( final IFile sldFile, final List< ? > collection, final QName geometryProperty, final QName styleProperty, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws IOException, SAXException, CoreException
  {
    final IProgressMonitor progressMonitor = monitor == null ? new NullProgressMonitor() : monitor;
    final StyledLayerDescriptor descriptor = createPolygonSLD( collection, geometryProperty, styleProperty, styleName, styleTitle, progressMonitor );
    exportSLD( sldFile, descriptor, progressMonitor );
  }

  public static void exportRasterSymbolyzerSLD( final IFile sldFile, final List< ? > collection, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws IOException, SAXException, CoreException
  {
    final IProgressMonitor progressMonitor = monitor == null ? new NullProgressMonitor() : monitor;
    final StyledLayerDescriptor descriptor = createRasterSLD( collection, styleName, styleTitle, progressMonitor );
    exportSLD( sldFile, descriptor, progressMonitor );
  }

  private static void exportSLD( final IFile sldFile, final StyledLayerDescriptor descriptor, final IProgressMonitor progressMonitor ) throws IOException, SAXException, CoreException
  {
    final ByteArrayInputStream stream = new ByteArrayInputStream( descriptor.exportAsXML().getBytes( "UTF-8" ) );
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
            factory.setAttribute( "indent-number", new Integer( 4 ) );
          }
          catch( final IllegalArgumentException e )
          {
          }

          final Transformer transformer = factory.newTransformer();
          transformer.setOutputProperty( OutputKeys.ENCODING, "UTF-8" ); //$NON-NLS-1$
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

  private static StyledLayerDescriptor createRasterSLD( final List< ? > collection, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws CoreException
  {
    final TreeMap<Double, ColorMapEntry> colorMap = new TreeMap<Double, ColorMapEntry>();
    final FeatureTypeStyle style = new FeatureTypeStyle_Impl();

    for( final Object styledFeatureObject : collection )
    {
      final IColorStyledFeatureWrapper styledFeature;
      if( styledFeatureObject instanceof IColorStyledFeatureWrapper )
        styledFeature = (IColorStyledFeatureWrapper) styledFeatureObject;
      else
        continue;
      if( monitor.isCanceled() )
        throw new CoreException( Status.CANCEL_STATUS );

      final RGB rgb = styledFeature.getColorStyle();
      Color color = null;
      if( rgb == null )
        color = Color.WHITE;
      else
        color = new Color( rgb.red, rgb.green, rgb.blue );
      final double quantity = styledFeature.getOrdinalNumber();
      final ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( color, 0.75, quantity, "" ); //$NON-NLS-1$
      colorMap.put( new Double( quantity ), colorMapEntry );
    }

    final RasterSymbolizer rasterSymbolizer = new RasterSymbolizer_Impl( colorMap );
    final Rule rule = StyleFactory.createRule( rasterSymbolizer );
    style.addRule( rule );

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final Style[] styles = new Style[] { new UserStyle_Impl( styleName, styleTitle, null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( LAYER_NAME, null, styles ) };
    return SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
  }

  private static StyledLayerDescriptor createPolygonSLD( final List< ? > collection, final QName geometryProperty, final QName styleProperty, final String styleName, final String styleTitle, final IProgressMonitor monitor ) throws CoreException
  {
    final FeatureTypeStyle style = new FeatureTypeStyle_Impl();

    // adding rules for every member
    for( final Object styledFeatureObject : collection )
    {
      final IColorStyledFeatureWrapper styledFeature;
      if( styledFeatureObject instanceof IColorStyledFeatureWrapper )
        styledFeature = (IColorStyledFeatureWrapper) styledFeatureObject;
      else
        continue;
      if( monitor.isCanceled() )
        throw new CoreException( Status.CANCEL_STATUS );

      final RGB rgb = styledFeature.getColorStyle();
      Color color = null;
      if( rgb == null )
        color = Color.WHITE;
      else
        color = new Color( rgb.red, rgb.green, rgb.blue );
      final Stroke stroke = StyleFactory.createStroke( Color.BLACK, 1.0, 0.5 );
      final Fill fill = StyleFactory.createFill( color, 0.75 );
      final PolygonSymbolizer newSymbolizer = StyleFactory.createPolygonSymbolizer( stroke, fill, new PropertyName( geometryProperty ) );
      final Rule rule = StyleFactory.createRule( newSymbolizer );
      final String ruleName = styledFeature.getName();
      final Operation operation = new PropertyIsLikeOperation( new PropertyName( styleProperty.getLocalPart(), null ), new Literal( ruleName ), '*', '$', '/' );
      final Filter filter = new ComplexFilter( operation );
      rule.setName( styledFeature.getGmlID() );
      rule.setTitle( ruleName );
      rule.setAbstract( ruleName );
      rule.setFilter( filter );
      style.addRule( rule );
    }

    final ParameterValueType[] anchorPoint = new ParameterValueType[2];
    anchorPoint[0] = StyleFactory.createParameterValueType( 0.5 );
    anchorPoint[1] = StyleFactory.createParameterValueType( 0.5 );
    final ParameterValueType rotation = StyleFactory.createParameterValueType( 0.0 );

    // adding labels rule
    final Font font = StyleFactory.createFont( "Arial", 11.0 );
    font.setColor( Color.BLACK );
    final PointPlacement pointPlacement = new PointPlacement_Impl( anchorPoint, new ParameterValueType[0], rotation, true );
    final LabelPlacement labelPlacement = new LabelPlacement_Impl( pointPlacement );
    final TextSymbolizer labelSymbolizer = StyleFactory.createTextSymbolizer( new PropertyName( geometryProperty ), "<ogc:PropertyName>" + styleProperty.getLocalPart() + "</ogc:PropertyName>", labelPlacement );
    labelSymbolizer.setHalo( null );
    labelSymbolizer.setFont( font );
    final Rule labelRule = StyleFactory.createRule( labelSymbolizer, 0.0, 10.0 );
    labelRule.setName( LABEL_RULE_NAME );
    labelRule.setTitle( LABEL_RULE_NAME );
    labelRule.setAbstract( LABEL_RULE_NAME );
    style.addRule( labelRule );

    // adding default rule
    final Stroke defaultRuleStroke = StyleFactory.createStroke( DEFAULT_RULE_STROKECOLOR, DEFAULT_RULE_STROKEWIDTH, DEFAULT_RULE_STROKEOPACITY );
    defaultRuleStroke.setDashArray( DEFAULT_RULE_DASHARRAY );
    final Fill defaultRuleFill = StyleFactory.createFill( DEFAULT_RULE_FILLCOLOR, DEFAULT_RULE_FILLOPACITY );
    final PolygonSymbolizer defaultRuleSymbolizer = StyleFactory.createPolygonSymbolizer( defaultRuleStroke, defaultRuleFill, new PropertyName( geometryProperty ) );
    final Rule defaultRule = StyleFactory.createRule( defaultRuleSymbolizer );
    defaultRule.setElseFilter( true );
    defaultRule.setName( DEFAULT_RULE_NAME );
    defaultRule.setTitle( DEFAULT_RULE_TITLE );
    defaultRule.setAbstract( DEFAULT_RULE_TITLE );
    style.addRule( defaultRule );

    final FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { style };
    final Style[] styles = new Style[] { new UserStyle_Impl( styleName, styleTitle, null, false, featureTypeStyles ) };
    final org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( LAYER_NAME, null, styles ) };
    return SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
  }
}
