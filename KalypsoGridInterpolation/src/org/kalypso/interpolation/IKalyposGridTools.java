/*
 * Created on 05.01.2005
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation;

import java.io.File;

import org.kalypso.interpolation.grid.IGrid;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * @author kuepfer TODO To change the template for this generated type comment go to Window - Preferences - Java - Code
 *         Style - Code Templates
 */
public interface IKalyposGridTools
{
  /**
   * This Method interpolates a mesh to a automatically generated Grid with a specific cell size.
   */
  public void interpolateGrid( Feature[] meshElements, Feature[] nodes, String geometryPropertyElement, String geometryPropertyPoint, String valueProperty, double cellsize, GM_Envelope gridsize, GM_Surface wishbox, GM_Surface polyline, String crs, File out ) throws Exception;

  public IGrid subtract( IGrid grid1, IGrid gird2 );

  public IGrid add( IGrid grid1, IGrid grid2 );

  public IGrid importGrid( File file, String cs ) throws Exception;
}