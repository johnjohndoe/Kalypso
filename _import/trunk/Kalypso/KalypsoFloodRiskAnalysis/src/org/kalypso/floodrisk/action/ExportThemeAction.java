package org.kalypso.floodrisk.action;

import java.awt.Color;
import java.awt.Point;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.renderable.ParameterBlock;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Interval;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

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

public class ExportThemeAction implements PluginMapOutlineAction
{

  private File m_targetFile;

  /**
   * @see org.kalypso.ogc.gml.outline.PluginMapOutlineAction#run(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void run( GisMapOutlineViewer outlineviewer )
  {
    final IKalypsoTheme activeTheme = outlineviewer.getMapModell().getActiveTheme();
    if( activeTheme instanceof GisTemplateFeatureTheme )
    {
      FeatureType featureType = ( (GisTemplateFeatureTheme)activeTheme ).getFeatureType();
      if( featureType.getName().equals( "RectifiedGridCoverage" ) )
      {
        //System.out.println( "CreateImage" );
        String targetFileName = chooseFile( outlineviewer.getControl().getShell(), m_targetFile,
            new String[]
            { "*.jpg",".JPG",".JPEG",".jpeg" } );
        if( targetFileName != null )
        {
          if( targetFileName.indexOf( "." ) == -1 )
            m_targetFile = new File( targetFileName + ".jpg" );
          else
            m_targetFile = new File( targetFileName );
        }
        if( m_targetFile != null )
        {
          Job createImageJob = new Job( "Create Image" )
          {

            protected IStatus run( IProgressMonitor monitor )
            {
              return createImage( (GisTemplateFeatureTheme)activeTheme, monitor );
            }
          };
          createImageJob.setUser( true );
          createImageJob.schedule();
        }
      }
      else
      {
        MessageDialog.openConfirm( outlineviewer.getControl().getShell(), "Information",
            "Export-Function not implemented" );
      }
    }
    else
    {
      MessageDialog.openConfirm( outlineviewer.getControl().getShell(), "Information",
          "Export-Function not implemented" );
    }
  }

  IStatus createImage( GisTemplateFeatureTheme activeTheme, IProgressMonitor monitor )
  {
    monitor.beginTask( "Create Image", IProgressMonitor.UNKNOWN );
    FeatureList featureList = activeTheme.getFeatureList();
    UserStyle[] style = activeTheme.getStyles();
    int pos = 0;
    RasterSymbolizer rasterSym = (RasterSymbolizer)style[pos].getFeatureTypeStyles()[pos]
        .getRules()[pos].getSymbolizers()[pos];
    RectifiedGridDomain rgDomain = (RectifiedGridDomain)( (Feature)featureList.get( pos ) )
        .getProperty( "rectifiedGridDomain" );
    RangeSet rangeSet = (RangeSet)( (Feature)featureList.get( pos ) ).getProperty( "rangeSet" );
    monitor.beginTask( "Create Image", IProgressMonitor.UNKNOWN );
    Raster raster = getSurrogateRaster( rgDomain, rangeSet, rasterSym, monitor );
    PlanarImage image = getSurrogateImage( raster );
    //File tempFile = new File( "D://temp//test.jpg" );
    monitor.setTaskName( "Save Image" );
    saveAsJpg( m_targetFile, image );
    File worldFile = new File( m_targetFile.getParentFile(), FileUtilities
        .nameWithoutExtension( m_targetFile.getName() )
        + ".jgw" );
    createWorldFile( worldFile, rgDomain );
    monitor.done();
    return Status.OK_STATUS;
  }

  private void createWorldFile( File worldFile, RectifiedGridDomain rgDomain )
  {
    try
    {
      BufferedWriter bw = new BufferedWriter( new FileWriter( worldFile ) );
      //dx
      double dx = rgDomain.getOffsetX(rgDomain.getOrigin(null).getCoordinateSystem());
      bw.write( "" + dx );
      bw.newLine();
      //phi x
      bw.write( "0.0" );
      bw.newLine();
      //phi y
      bw.write( "0.0" );
      bw.newLine();
      //dy
      double dY = rgDomain.getOffsetY(rgDomain.getOrigin(null).getCoordinateSystem());
      bw.write( "-" + dY );
      bw.newLine();
      //origin x (upper left corner)
      double originX = rgDomain.getOrigin( null ).getX();
      bw.write( "" + originX );
      bw.newLine();
      //origin y (upper left corner)
      GM_Envelope envelope = rgDomain.getGM_Envelope( rgDomain.getOrigin( null )
          .getCoordinateSystem() );
      double originY = envelope.getMax().getY();
      bw.write( "" + originY );
      bw.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * saves a surrogateImage as JPG
   * 
   * @param outFile
   *          file for saving the image
   */
  void saveAsJpg( File outFile, PlanarImage surrogateImage )
  {
    try
    {
      JAI.create( "filestore", surrogateImage, outFile.getAbsolutePath(), "jpeg" );

    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  String chooseFile( Shell parentShell, File selectedFile, String[] filterExtensions )
  {
    FileDialog dialog = new FileDialog( parentShell, SWT.SINGLE );
    dialog.setFilterExtensions( filterExtensions );
    if( selectedFile != null )
    {
      dialog.setFileName( selectedFile.getName() );
      dialog.setFilterPath( selectedFile.getParent() );
    }
    dialog.open();
    String fileName = dialog.getFileName();
    String filterPath = dialog.getFilterPath();
    String filePath = null;
    if( fileName != null && fileName != "" && filterPath != null )
    {
      filePath = filterPath + "/" + fileName;
    }
    return filePath;
  }

  private PlanarImage getSurrogateImage( Raster surrogateRaster )
  {
    PlanarImage surrogateImage = getPlanarImage( surrogateRaster );
    //       Let's convert the data type for displaying.
    ParameterBlock pbConvert = new ParameterBlock();
    pbConvert.addSource( surrogateImage );
    pbConvert.add( DataBuffer.TYPE_BYTE );
    surrogateImage = JAI.create( "format", pbConvert );
    return surrogateImage;
  }

  private PlanarImage getPlanarImage( Raster raster )
  {
    ColorModel colorModel = PlanarImage.createColorModel( raster.getSampleModel() );
    TiledImage tiledImage = new TiledImage( 0, 0, raster.getWidth(), raster.getHeight(), 0, 0,
        raster.getSampleModel(), colorModel );
    tiledImage.setData( raster );
    return tiledImage;
  }

  private Raster getSurrogateRaster( RectifiedGridDomain gridDomain, RangeSet rangeSet,
      RasterSymbolizer rasterSym, IProgressMonitor monitor )
  {
    final int mode_intervalColorMapping = 0;
    final int mode_valueColorMapping = 1;
    int mode = rasterSym.getMode();
    TreeMap intervalMap = null;
    if( mode == mode_intervalColorMapping )
    {
      intervalMap = rasterSym.getIntervalMap();
    }

    int nCols = gridDomain.getNumColumns();
    int nRows = gridDomain.getNumRows();
    SampleModel sampleModel = RasterFactory.createBandedSampleModel( DataBuffer.TYPE_INT, nCols,
        nRows, 3 ); //4 );
    DataBuffer dataBuffer = sampleModel.createDataBuffer();
    Vector rangeSetData = rangeSet.getRangeSetData();
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rangeSetDataRow = (Vector)rangeSetData.get( i );
      for( int j = 0; j < rangeSetDataRow.size(); j++ )
      {
        Color actualColor = Color.DARK_GRAY;
        //double actualOpacity = 1;
        if( rangeSetDataRow.get( j ) != null )
        {
          double actualValue = ( (Double)rangeSetDataRow.get( j ) ).doubleValue();
          switch( mode )
          {
          case mode_intervalColorMapping:
          {
            Iterator it = intervalMap.keySet().iterator();
            while( it.hasNext() )
            {
              Interval interval = (Interval)it.next();
              if( interval.contains( actualValue ) )
              {
                actualColor = (Color)intervalMap.get( interval );
                //actualOpacity = actualColor.getAlpha();
                break;
              }
            }
            break;
          }
          case mode_valueColorMapping:
          {
            TreeMap colorMap = rasterSym.getColorMap();
            if( colorMap.containsKey( new Double( actualValue ) ) )
            {
              ColorMapEntry colorMapEntry = (ColorMapEntry)colorMap.get( new Double( actualValue ) );
              actualColor = colorMapEntry.getColor();
              //actualOpacity = colorMapEntry.getOpacity();
            }
            break;
          }
          }
        }
        else
        {
          TreeMap colorMap = rasterSym.getColorMap();
          double nullValue = -9999;
          if( colorMap.containsKey( new Double( nullValue ) ) )
          {
            ColorMapEntry colorMapEntry = (ColorMapEntry)colorMap.get( new Double( nullValue ) );
            actualColor = colorMapEntry.getColor();
            //actualOpacity = colorMapEntry.getOpacity();
          }
          else
          {
            actualColor = Color.WHITE;
            //actualOpacity = 0;
          }
        }
        int redValue = actualColor.getRed();
        int greenValue = actualColor.getGreen();
        int blueValue = actualColor.getBlue();
        //int alphaValue = (int)Math.round( actualOpacity * 255 );
        dataBuffer.setElem( 0, j + ( i * nCols ), redValue );
        dataBuffer.setElem( 1, j + ( i * nCols ), greenValue );
        dataBuffer.setElem( 2, j + ( i * nCols ), blueValue );
        //dataBuffer.setElem( 3, j + ( i * nCols ), alphaValue );
      }
      monitor.setTaskName( i + " rows of " + rangeSetData.size() + " calculated." );
      //monitor.worked( i );
    }
    Point origin = new Point( 0, 0 );
    Raster surrogateRaster = RasterFactory.createWritableRaster( sampleModel, dataBuffer, origin );
    return surrogateRaster;
  }

  public void selectionChanged( PluginMapOutlineActionDelegate action, ISelection selection )
  {
    // TODO Auto-generated method stub
    
  }

}