package org.kalypso.loader.impl;

import java.io.File;
import java.util.Properties;

import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.runtime.UtilProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
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
      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();

      final CS_CoordinateSystem sourceCrs = org.deegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( sourceSrs ) );
      
      if( sourceCrs == null )
        throw new LoaderException( "Kein Koordinaten-System für Shape gefunden: " + sourceSrs );

      final KalypsoFeatureLayer layer = ShapeSerializer.deserialize( sourceFile.getAbsolutePath(), sourceCrs, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), new UtilProgressMonitor( monitor ) );
      
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