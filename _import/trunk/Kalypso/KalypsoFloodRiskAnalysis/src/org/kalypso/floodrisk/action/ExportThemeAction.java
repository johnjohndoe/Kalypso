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
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.floodrisk.internationalize.Messages;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Interval;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector;

/**
 * 
 * ExportThemeAction
 * <p>
 * Action to export a rasterTheme as jpg-image with world-file
 * 
 * created by
 * 
 * @author Nadja Peiler (14.06.2005)
 */
public class ExportThemeAction implements PluginMapOutlineAction
{

  private File m_targetFile;

  /**
   * @see org.kalypso.ogc.gml.outline.PluginMapOutlineAction#run(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void run( IAction action )
  {
    if( action instanceof PluginMapOutlineActionDelegate )
    {
      PluginMapOutlineActionDelegate outlineaction = (PluginMapOutlineActionDelegate)action;
      IMapModellView viewer = outlineaction.getOutlineviewer();
      final IKalypsoTheme activeTheme = viewer.getMapModell().getActiveTheme();
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final Shell activeShell = workbench.getDisplay().getActiveShell();
      if( activeTheme instanceof GisTemplateFeatureTheme )
      {
        IFeatureType featureType = ( (GisTemplateFeatureTheme)activeTheme ).getFeatureType();
        if( featureType.getName().equals( "RectifiedGridCoverage" ) ) //$NON-NLS-1$
        {
          //System.out.println( "CreateImage" );
          String targetFileName = chooseFile( activeShell, m_targetFile, new String[]
          {
              "*.jpg", //$NON-NLS-1$
              ".JPG", //$NON-NLS-1$
              ".JPEG", //$NON-NLS-1$
              ".jpeg" } ); //$NON-NLS-1$
          if( targetFileName != null )
          {
            if( targetFileName.indexOf( "." ) == -1 ) //$NON-NLS-1$
              m_targetFile = new File( targetFileName + ".jpg" ); //$NON-NLS-1$
            else
              m_targetFile = new File( targetFileName );
          }
          if( m_targetFile != null )
          {
            Job createImageJob = new Job( Messages.getString("action.ExportThemeAction.CreateImage") ) //$NON-NLS-1$
            {
              @Override
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
          MessageDialog.openConfirm( activeShell, Messages.getString("action.ExportThemeAction.Information"), Messages.getString("action.ExportThemeAction.ExportFunctionNotImplemented") ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
      else
      {
        MessageDialog.openConfirm( activeShell, Messages.getString("action.ExportThemeAction.Information"), Messages.getString("action.ExportThemeAction.ExportFunctionNotImplemented") ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
  }

  /**
   * creates the image and saves it as jpg to m_targetFile
   * 
   * @param activeTheme
   *          theme with featureTypeName = "RectifiedGridCoverage"
   * @param monitor
   *  
   */
  IStatus createImage( GisTemplateFeatureTheme activeTheme, IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString("action.ExportThemeAction.CreateImage"), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    FeatureList featureList = activeTheme.getFeatureList();
    UserStyle[] style = activeTheme.getStyles();
    int pos = 0;
    RasterSymbolizer rasterSym = (RasterSymbolizer)style[pos].getFeatureTypeStyles()[pos].getRules()[pos]
        .getSymbolizers()[pos];
    RectifiedGridDomain rgDomain = (RectifiedGridDomain)( (Feature)featureList.get( pos ) )
        .getProperty( "rectifiedGridDomain" ); //$NON-NLS-1$
    RangeSet rangeSet = (RangeSet)( (Feature)featureList.get( pos ) ).getProperty( "rangeSet" ); //$NON-NLS-1$
    monitor.beginTask( Messages.getString("action.ExportThemeAction.CreateImage"), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    Raster raster = getSurrogateRaster( rgDomain, rangeSet, rasterSym, monitor );
    PlanarImage image = getSurrogateImage( raster );
    //File tempFile = new File( "D://temp//test.jpg" );
    monitor.setTaskName( Messages.getString("action.ExportThemeAction.SaveImage") ); //$NON-NLS-1$
    saveAsJpg( m_targetFile, image );
    File worldFile = new File( m_targetFile.getParentFile(), FileUtilities
        .nameWithoutExtension( m_targetFile.getName() )
        + ".jgw" ); //$NON-NLS-1$
    createWorldFile( worldFile, rgDomain );
    monitor.done();
    return Status.OK_STATUS;
  }

  /**
   * writes a worldFile with the geometric information of the RectifiedGridDomain
   * 
   * @param worldFile
   * @param rgDomain
   *          RectifiedGridDomain of a RectifiedGridCoverage
   *  
   */
  private void createWorldFile( File worldFile, RectifiedGridDomain rgDomain )
  {
    try
    {
      final BufferedWriter bw = new BufferedWriter( new FileWriter( worldFile ) );
      
      final OffsetVector offsetX = rgDomain.getOffsetX();
      final OffsetVector offsetY = rgDomain.getOffsetX();
      
      final GM_Point origin = rgDomain.getOrigin( null );

      //dx
      bw.write( "" + offsetX.getGeoX() ); //$NON-NLS-1$
      bw.newLine();
      //phi x
      bw.write( "" + offsetX.getGeoY() ); //$NON-NLS-1$
      bw.newLine();
      //phi y
      bw.write( "" + offsetY.getGeoX() ); //$NON-NLS-1$
      bw.newLine();
      //dy
      bw.write( "" + offsetY.getGeoY() ); //$NON-NLS-1$
      bw.newLine();
      //origin x (upper left corner)
      bw.write( "" + origin.getX() ); //$NON-NLS-1$
      bw.newLine();
      //origin y (upper left corner)
      bw.write( "" + origin.getY() ); //$NON-NLS-1$
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
   * @param surrogateImage
   *          surrogate image of RectifiedGridCoverage (no transparency!)
   *  
   */
  void saveAsJpg( File outFile, PlanarImage surrogateImage )
  {
    try
    {
      JAI.create( "filestore", surrogateImage, outFile.getAbsolutePath(), "jpeg" ); //$NON-NLS-1$ //$NON-NLS-2$

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
    if( fileName != null && fileName != "" && filterPath != null ) //$NON-NLS-1$
    {
      filePath = filterPath + "/" + fileName; //$NON-NLS-1$
    }
    return filePath;
  }

  /**
   * get a surrogate image for displaying with byte values of the given raster with int values
   * 
   * @param surrogateRaster
   * @return PlanarImage (surrogate image)
   */
  private PlanarImage getSurrogateImage( Raster surrogateRaster )
  {
    PlanarImage surrogateImage = getPlanarImage( surrogateRaster );
    //       Let's convert the data type for displaying.
    ParameterBlock pbConvert = new ParameterBlock();
    pbConvert.addSource( surrogateImage );
    pbConvert.add( DataBuffer.TYPE_BYTE );
    surrogateImage = JAI.create( "format", pbConvert ); //$NON-NLS-1$
    return surrogateImage;
  }

  /**
   * get an image with the given Raster-Object
   * 
   * @param raster
   * @return Image
   */
  private PlanarImage getPlanarImage( Raster raster )
  {
    ColorModel colorModel = PlanarImage.createColorModel( raster.getSampleModel() );
    TiledImage tiledImage = new TiledImage( 0, 0, raster.getWidth(), raster.getHeight(), 0, 0, raster.getSampleModel(),
        colorModel );
    tiledImage.setData( raster );
    return tiledImage;
  }

  /**
   * creates a surrogate raster of the given rectifiedGridDomain and rangeSet with the given colorTable in the
   * rasterSymbolizer
   */
  private Raster getSurrogateRaster( RectifiedGridDomain gridDomain, RangeSet rangeSet, RasterSymbolizer rasterSym,
      IProgressMonitor monitor )
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
    SampleModel sampleModel = RasterFactory.createBandedSampleModel( DataBuffer.TYPE_INT, nCols, nRows, 3 ); //4 );
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
      monitor.setTaskName( i + " "+Messages.getString("action.ExportThemeAction.RowsOf")+" " + rangeSetData.size() + " "+Messages.getString("action.ExportThemeAction.Calculated")+"." ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      //monitor.worked( i );
    }
    Point origin = new Point( 0, 0 );
    Raster surrogateRaster = RasterFactory.createWritableRaster( sampleModel, dataBuffer, origin );
    return surrogateRaster;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // nothing

  }

}