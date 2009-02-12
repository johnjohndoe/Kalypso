/*
 * Created on 18.01.2005
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.grid;

import java.io.File;

import javax.naming.OperationNotSupportedException;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * @author kuepfer TODO To change the template for this generated type comment go to Window - Preferences - Java - Code
 *         Style - Code Templates
 */
public interface IGrid
{
  public static String DEFAULT_NO_DATA = "-9999";

  public String getGridID( );

  public GM_Surface getExtend( );

  public IGrid merge( IGrid grid1, IGrid grid2 );

  public IGrid tile( IGrid gird, GM_Envelope tilesize ) throws OperationNotSupportedException;

  public GM_Position[] getGridCells( ) throws GM_Exception;

  public GM_Position getOrigin( );

  public double getCellSize( );

  public boolean isPointOnGrid( GM_Position pos, boolean inEnvelope );

  public String getCoordinateSystem( );

  public int getRows( );

  public int getCols( );

  public void export( File file );

}
