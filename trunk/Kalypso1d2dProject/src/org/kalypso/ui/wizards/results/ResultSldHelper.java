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
package org.kalypso.ui.wizards.results;

import java.awt.Color;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.filters.StringInputStream;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.LineColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author Thomas Jung
 * 
 */
public class ResultSldHelper
{
  
  /**
   * returns the interpolated color of a color map defined by start and end color.
   * 
   * @param currentClass
   *          current class
   * @param numOfClasses
   *          number of all classes in which the colormap is divided.
   */
  public static Color interpolateColor( final Color minColor, final Color maxColor, final int currentClass, final int numOfClasses )
  {
    // interpolate color
    final float[] minhsb = Color.RGBtoHSB( minColor.getRed(), minColor.getGreen(), minColor.getBlue(), null );
    final float[] maxhsb = Color.RGBtoHSB( maxColor.getRed(), maxColor.getGreen(), maxColor.getBlue(), null );

    final float minHue = minhsb[0];
    final float maxHue = maxhsb[0];

    final float minSat = minhsb[1];
    final float maxSat = maxhsb[1];

    final float minBri = minhsb[2];
    final float maxBri = maxhsb[2];

    final double Hue = minHue + (currentClass * (maxHue - minHue) / (numOfClasses - 1));
    final double Sat = minSat + (currentClass * (maxSat - minSat) / (numOfClasses - 1));
    final double Bri = minBri + (currentClass * (maxBri - minBri) / (numOfClasses - 1));

    final Color hsbColor = Color.getHSBColor( (float) Hue, (float) Sat, (float) Bri );
    final Color rgbColor = new Color( hsbColor.getRed(), hsbColor.getGreen(), hsbColor.getBlue() );

    return rgbColor;
  }

  public static double interpolate( final double min, final double max, final int currentClass, final int numOfClasses )
  {
    return min + (currentClass * (max - min) / (numOfClasses - 1));
  }

  public static IStatus processStyle( final IFile styleFile, final IFolder sldFolder, final String type, final BigDecimal minValue, final BigDecimal maxValue )
  {
    /* Read sld from template */

    // better, get the sld from resource folder
    final URL resource = ResultSldHelper.class.getResource( "resources/myDefault" + type + ".sld" ); //$NON-NLS-1$ //$NON-NLS-2$
    try
    {
      final StyledLayerDescriptor sld = SLDFactory.createSLD( resource );

      if( NodeResultHelper.LINE_TYPE.equals( type ) || NodeResultHelper.POLYGON_TYPE.equals( type ) )
      {
        //resolve the problem with initial creation of style files in case with min equals max
        BigDecimal lMax = maxValue;
        if( minValue.equals( maxValue ) ){
          lMax = maxValue.add( new BigDecimal( 1 ) );
        }
        processTinStyle( type, minValue, lMax, sld );
        /* Write SLD back to file */
        if( sld != null )
        {
          final String sldXML = sld.exportAsXML();
          final String sldXMLwithHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + sldXML; //$NON-NLS-1$
          styleFile.create( new StringInputStream( sldXMLwithHeader, "UTF-8" ), false, new NullProgressMonitor() ); //$NON-NLS-1$
        }
      }
      else if( NodeResultHelper.NODE_TYPE.equals( type ) )
      {
        // we creating more styles for nodes to give the possibility for representation of different types of node
        // results
        createDefaultNodeStyles( sldFolder, maxValue, resource, type );
        // String sldXMLwithHeader = processVectorStyle( maxValue, resource, type ); styleFile,
        // styleFile.create( new StringInputStream( sldXMLwithHeader ), false, new NullProgressMonitor() );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ui.wizards.results.ResultSldHelper.0" ) ); //$NON-NLS-1$
    }
    return Status.OK_STATUS;

  }

  private static void createDefaultNodeStyles( final IFolder sldFolder, final BigDecimal maxValue, final URL resource, final String type )
  {
    for( int i = 0; i < NodeResultHelper.NodeStyleTypes.length; i++ )
    {
      final String sldFileName = ResultMeta1d2dHelper.getDefaultStyleFileName( type, NodeResultHelper.NodeStyleTypes[i] );
      final IFile styleFile = sldFolder.getFile( sldFileName );
      String sldXMLwithHeader = processVectorStyle( maxValue, resource, NodeResultHelper.NodeStyleTypes[i] );
      try{
        styleFile.create( new StringInputStream( sldXMLwithHeader ), false, new NullProgressMonitor() );
      }
      catch (Exception e) {
        //do not replace existing style files
      }
    }
  }

  private static void processTinStyle( final String type, final BigDecimal minValue, final BigDecimal maxValue, final StyledLayerDescriptor sld )
  {
    final String layerName = "tin" + type + "Style"; //$NON-NLS-1$ //$NON-NLS-2$
    final String featureTypeStyleName = "tinFeatureTypeStyle"; //$NON-NLS-1$
    final String ruleName = "tinRule"; //$NON-NLS-1$
 
    final NamedLayer namedLayer = sld.getNamedLayer( "tinStyles" ); //$NON-NLS-1$
    if( namedLayer == null )
      return;

    final Style style = namedLayer.getStyle( layerName );
    if( style instanceof UserStyle )
    {
      final UserStyle userStyle = (UserStyle) style;
      final FeatureTypeStyle featureTypeStyle = userStyle.getFeatureTypeStyle( featureTypeStyleName );
      if( featureTypeStyle == null )
        return;
      final Rule rule = featureTypeStyle.getRule( ruleName );
      if( rule == null )
        return;
      final Symbolizer[] symbolizers = rule.getSymbolizers();

      // get the first symbolizer
      final Symbolizer symbolizer = symbolizers[0];

      try
      {
        if( symbolizer instanceof SurfaceLineSymbolizer )
        {
          configureLineSymbolizer( (SurfaceLineSymbolizer) symbolizer, minValue, maxValue );
        }
        else if( symbolizer instanceof SurfacePolygonSymbolizer )
        {
          configurePolygonSymbolizer( (SurfacePolygonSymbolizer) symbolizer, minValue, maxValue );
        }
      }
      catch( final FilterEvaluationException e )
      {
        e.printStackTrace();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

    }
  }

  private static void configurePolygonSymbolizer( final SurfacePolygonSymbolizer symbolizer, final BigDecimal minValue, final BigDecimal maxValue ) throws FilterEvaluationException
  {
    final PolygonColorMap templateColorMap = symbolizer.getColorMap();
    final PolygonColorMap newColorMap = new PolygonColorMap_Impl();

    // retrieve stuff from template-entries
    final PolygonColorMapEntry fromEntry = templateColorMap.findEntry( "from", null ); //$NON-NLS-1$
    final PolygonColorMapEntry toEntry = templateColorMap.findEntry( "to", null ); //$NON-NLS-1$

    // Fill
    final Color fromPolygonColor = fromEntry.getFill().getFill( null );
    final Color toPolygonColor = toEntry.getFill().getFill( null );
    final double polygonOpacity = fromEntry.getFill().getOpacity( null );

    // Stroke
    final Color fromLineColor = fromEntry.getStroke().getStroke( null );
    final Color toLineColor = toEntry.getStroke().getStroke( null );
    final double lineOpacity = fromEntry.getStroke().getOpacity( null );

    // step width
    final double stepWidth = fromEntry.getTo( null );

    // scale of the step width
    final BigDecimal setScale = new BigDecimal( fromEntry.getFrom( null ) ).setScale( 0, BigDecimal.ROUND_FLOOR );
    final int stepWidthScale = setScale.intValue();

    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isareas by using class steps of 0.1
    // later, the classes will be created by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = minValue.setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = maxValue.setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal polygonStepWidth = new BigDecimal( stepWidth ).setScale( stepWidthScale, BigDecimal.ROUND_FLOOR );
    int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( polygonStepWidth )).intValue();
    // set to provide more them 1 or 0 classes. in such cases the color map will not be created, that results error.  
    if( numOfClasses < 2 ){
      numOfClasses = (maxDecimal.subtract( minDecimal ).divide( polygonStepWidth.divide( new BigDecimal( 4 ) ) ) ).intValue();
    }
    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double fromValue = minDecimal.doubleValue() + currentClass * polygonStepWidth.doubleValue();
      final double toValue = minDecimal.doubleValue() + (currentClass + 1) * polygonStepWidth.doubleValue();

      // Stroke
      Color lineColor;
      if( fromLineColor == toLineColor )
        lineColor = fromLineColor;
      else
        lineColor = interpolateColor( fromLineColor, toLineColor, currentClass, numOfClasses );

      // Fill
      final Color polygonColor = interpolateColor( fromPolygonColor, toPolygonColor, currentClass, numOfClasses );
      lineColor = polygonColor;

      final Stroke stroke = StyleFactory.createStroke( lineColor, lineOpacity, 1 );

      final Fill fill = StyleFactory.createFill( polygonColor, polygonOpacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isofl‰che " + currentClass ); //$NON-NLS-1$
      final ParameterValueType from = StyleFactory.createParameterValueType( fromValue );
      final ParameterValueType to = StyleFactory.createParameterValueType( toValue );

      final PolygonColorMapEntry colorMapEntry = new PolygonColorMapEntry_Impl( fill, stroke, label, from, to );
      newColorMap.addColorMapClass( colorMapEntry );
    }

    symbolizer.setColorMap( newColorMap );
  }

  /**
   * sets the parameters for the colormap of an isoline
   */
  private static void configureLineSymbolizer( final SurfaceLineSymbolizer symbolizer, final BigDecimal minValue, final BigDecimal maxValue ) throws FilterEvaluationException
  {
    final LineColorMap templateColorMap = symbolizer.getColorMap();
    final LineColorMap newColorMap = new LineColorMap_Impl();

    // retrieve stuff from template-entries
    final LineColorMapEntry fromEntry = templateColorMap.findEntry( "from", null ); //$NON-NLS-1$
    final LineColorMapEntry toEntry = templateColorMap.findEntry( "to", null ); //$NON-NLS-1$
    final LineColorMapEntry fatEntry = templateColorMap.findEntry( "fat", null ); //$NON-NLS-1$

    final Color fromColor = fromEntry.getStroke().getStroke( null );
    final Color toColor = toEntry.getStroke().getStroke( null );
    final double opacity = fromEntry.getStroke().getOpacity( null );

    final double normalWidth = fromEntry.getStroke().getWidth( null );
    final double fatWidth = fatEntry.getStroke().getWidth( null );

    // defines which isolines are drawn with a fat line
    final double fatValue = fatEntry.getQuantity( null );
    // TODO: get setep / scale from quantity
    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isolines using class steps of 0.1
    // later, the classes will be done by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = minValue.setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = maxValue.setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal stepWidth = new BigDecimal( 0.1 ).setScale( 1, BigDecimal.ROUND_HALF_UP );
    final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( stepWidth )).intValue() + 1;

    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double currentValue = minDecimal.doubleValue() + currentClass * stepWidth.doubleValue();

      Color lineColor;
      if( fromColor == toColor )
        lineColor = fromColor;
      else
        lineColor = interpolateColor( fromColor, toColor, currentClass, numOfClasses );

      final double strokeWidth;
      if( currentValue % fatValue == 0 )
        strokeWidth = fatWidth;
      else
        strokeWidth = normalWidth;

      final Stroke stroke = StyleFactory.createStroke( lineColor, strokeWidth, opacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isolinie " + currentClass ); //$NON-NLS-1$
      final ParameterValueType quantity = StyleFactory.createParameterValueType( currentValue );

      final LineColorMapEntry colorMapEntry = new LineColorMapEntry_Impl( stroke, label, quantity );
      newColorMap.addColorMapClass( colorMapEntry );
    }

    symbolizer.setColorMap( newColorMap );
  }

  private static String processVectorStyle( final BigDecimal maxValue, final URL url, final String type )
  {
    InputStream is = null;
    try
    {
      if( NodeResultHelper.VELO_TYPE.equals( type ) || NodeResultHelper.WAVE_DIRECTION_TYPE.equals( type ) )
      {
        is = new BufferedInputStream( url.openStream() );
      }
      else
      {
        final URL extUrl = ResultSldHelper.class.getResource( "resources/myDefaultNodeExt.sld" ); //$NON-NLS-1$
        is = new BufferedInputStream( extUrl.openStream() );
      }
      String fileString = IOUtils.toString( is, "UTF-8" ); //$NON-NLS-1$
      is.close();

      // we assume, that the mean distance of mesh nodes is about 30 m, so that the vectors are expanded by an factor
      // which delivers vector lengths of 30 m as maximum.
      BigDecimal factorValue;
      if( maxValue != null && maxValue.doubleValue() > 0 )
        factorValue = new BigDecimal( 30 / maxValue.doubleValue() ).setScale( 0, BigDecimal.ROUND_CEILING );
      else
        factorValue = new BigDecimal( 100 ).setScale( 0, BigDecimal.ROUND_CEILING );

      String factor = factorValue.toString();
      if( NodeResultHelper.VELO_TYPE.equals( type ) ) { 
        return fileString.replaceAll( NodeResultHelper.VECTORFACTOR, factor ).replaceAll( NodeResultHelper.SIZE_NORM_NODE_FUNC, "velocityNorm" ); //$NON-NLS-1$
      }
      else if( NodeResultHelper.WAVE_DIRECTION_TYPE.equals( type ) ){
        return fileString.replaceAll( NodeResultHelper.VECTORFACTOR, factor ).replaceAll( NodeResultHelper.SIZE_NORM_NODE_FUNC, "wavehsig" ).replaceAll( "velocityRotation", type.toLowerCase() ); //$NON-NLS-1$ //$NON-NLS-2$ 
      }
      else
      {
        if( factorValue.doubleValue() > 30 ){
          factor = "10"; //$NON-NLS-1$ 
        }
        return fileString.replaceAll( NodeResultHelper.VECTORFACTOR, factor ).replaceAll( NodeResultHelper.NODESTYLE_TEMPLATE, type.toLowerCase() );
      }

    }
    catch( final IOException e )
    {
      e.printStackTrace();

    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
    return null;

  }

  public static boolean allDefaultNodeStylesExist( final IFolder sldFolder )
  {
    for( int i = 0; i < NodeResultHelper.NodeStyleTypes.length; i++ )
    {
      final String sldFileName = ResultMeta1d2dHelper.getDefaultStyleFileName( NodeResultHelper.NODE_TYPE, NodeResultHelper.NodeStyleTypes[i] );
      final IFile styleFile = sldFolder.getFile( sldFileName );
      if( !styleFile.exists() )
        return false;
      
    }
    return true;
  }

}
