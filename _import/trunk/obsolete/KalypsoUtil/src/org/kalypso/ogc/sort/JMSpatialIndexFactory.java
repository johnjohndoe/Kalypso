package org.kalypso.ogc.sort;

import org.deegree.model.sort.JMSpatialIndex;

public class JMSpatialIndexFactory
{
  public static JMSpatialIndex createSpatialIndex()
  {
    return new SplitSort();
  }
}