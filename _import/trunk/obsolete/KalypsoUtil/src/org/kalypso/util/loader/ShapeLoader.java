package org.kalypso.util.loader;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author schlienger
 *  
 */
public class ShapeLoader implements ILoader
{
  public ShapeLoader()
  {
  // nothing
  }

  /**
   * @see org.kalypso.util.loader.ILoader#load(java.lang.String)
   */
  public Object load( String source ) throws LoaderException
  {
    try
    {
      final ShapeFile sf = new ShapeFile( source );
      int count = sf.getRecordNum();
      FeatureCollection fc = FeatureFactory.createFeatureCollection( "test", count );

      for( int i = 0; i < count; i++ )
      {
        Feature fe = sf.getFeatureByRecNo( i + 1 );

        fc.appendFeature( fe );
      }

      sf.close();

      return fc;
    }
    catch( Exception e )
    {
      throw new LoaderException( e );
    }
  }
}