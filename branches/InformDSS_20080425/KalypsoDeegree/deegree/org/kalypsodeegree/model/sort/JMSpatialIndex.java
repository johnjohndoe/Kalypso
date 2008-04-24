package org.kalypsodeegree.model.sort;

import java.awt.Graphics;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

public interface JMSpatialIndex<T>
{
  public boolean add( final T object );

  public List<T> query( final GM_Envelope env, final List<T> result );

  public List<T> query( final GM_Position env, final List<T> result );

  public boolean remove( final Object object );

  /** Invalidate the spatial index. The next time one of the 'query' methods is called, a resort is made. */
  public void invalidate( );

  /**
   * Invalidate the spatial index. The next time one of the 'query' methods is called, a resort is made.
   * 
   * @param o
   *            Only this object is invalid. Implementors may use this information to improve the resort performance.
   */
  public void invalidate( final Object o );

  /** Paints the quad-tree as rectangles */
  public void paint( final Graphics g, final GeoTransform geoTransform );

  public GM_Envelope getBoundingBox( );
}