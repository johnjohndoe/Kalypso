package org.kalypso.loader.impl;

import java.io.File;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author schlienger
 *  
 */
public class ShapeLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final Properties source, final IProject project, final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String sourceType = source.getProperty( "SERVICE", "FILE" );
      final String sourcePath = source.getProperty( "PATH", "" );

      IFile shpResource = null;
      IFile dbfResource = null;
      IFile shxResource = null;
      
      File sourceFile = null;
      if( sourceType.equals( "FILE" ) )
        sourceFile = new File( sourcePath );
      else
      //RESOURCE
      {
        final IFile resource = project.getFile( sourcePath );
        sourceFile = resource.getLocation().toFile();

        shpResource = project.getFile( sourcePath + ".shp" );
        dbfResource = project.getFile( sourcePath + ".dbf" );
        shxResource = project.getFile( sourcePath + ".shx" );
      }

      final String sourceSrs = source.getProperty( "SRS", "" );

      final ShapeFile sf = new ShapeFile( sourceFile.getAbsolutePath() );

      final int count = sf.getRecordNum();

      final FeatureType featureType = sf.getFeatureByRecNo( 1 ).getFeatureType();
      final String name = source + featureType.getName();
      final KalypsoFeatureLayer layer = new KalypsoFeatureLayer( name, featureType,
          KalypsoGisPlugin.getDefault().getCoordinatesSystem() );

      // die shape-api liefert stets WGS84 als Koordinatensystem, daher
      // Anpassung hier:

      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();

      final CS_CoordinateSystem srcCS = org.deegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( sourceSrs ) );
      //      final int max= count < 20 ? count:20;
      final int max = count; // < 20 ? count:20;
      if( max < count ) 
        System.out.println( "WARNUNG es werden nur " + max + " von " + count + " Features geladen" );

      monitor.beginTask( "Geometrien werden geladen...", max + 1 );
      
      for( int i = 0; i < max; i++ )
      {
        final Feature fe = sf.getFeatureByRecNo( i + 1 );
        GMLHelper.setCrs( fe, srcCS );
        if( fe != null )
          layer.addFeature( new KalypsoFeature( fe ) );
        
        monitor.worked(i);
      }

      sf.close();
      
      monitor.setTaskName( "Layer wird optimiert..." );
      layer.optimize();
      
      monitor.worked( max );

      if( shpResource != null )
        addResource( shpResource, layer );
      if( dbfResource != null )
        addResource( dbfResource, layer );
      if( shxResource != null )
        addResource( shxResource, layer );

      return layer;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }
}