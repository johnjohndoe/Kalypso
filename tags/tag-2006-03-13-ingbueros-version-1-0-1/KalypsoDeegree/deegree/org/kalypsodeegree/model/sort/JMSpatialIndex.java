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

  public List<T> queryAll( final List<T> result );

  public boolean remove( final Object object );

  public void resort();

  public void paint( final Graphics g, final GeoTransform geoTransform );

  /** TODO: Was bedeutet das??? */
  public int rsize();

  public GM_Envelope getBoundingBox();
}