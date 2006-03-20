package org.kalypsodeegree.model.sort;

import java.awt.Graphics;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

public interface JMSpatialIndex
{
  public boolean add( Object object );

  public List query( GM_Envelope env, List result );

  public List query( GM_Position env, List result );

  public List queryAll( List result );

  public boolean remove( Object object );

  public void resort();

  public void paint( Graphics g, GeoTransform geoTransform );

  /** TODO: Was bedeutet das??? */
  public int rsize();

  public GM_Envelope getBoundingBox();
}