/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor;

import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * Utilities around IObservation.
 * 
 * @author schlienger
 */
public class ObservationUtilities
{
  private static final String MSG_ERROR_NOAXISTYPE = "Keine Achse gefunden vom Typ: ";
  private static final Comparator AXIS_SORT_COMPARATOR = new AxisSortComparator();

  private ObservationUtilities( )
  {
    //  not intended to be instanciated
  }

  /**
   * Finds the axis of the given observation that has the given name.
   * 
   * @param axes
   *          the list of axes to scan
   * @param axisName
   *          the name of the axis which is searched
   * @return first axis found
   * @throws NoSuchElementException
   *           when no axis matches the name
   */
  public static IAxis findAxisByName( final IAxis[] axes, final String axisName )
      throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getName().equalsIgnoreCase( axisName ) )
        return axes[i];
    }

    throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + axisName );
  }

  /**
   * returns null when no axis found instead of throwing an exception
   * 
   * @return axis or null if not found
   * 
   * @see ObservationUtilities#findAxisByName(IAxis[], String)
   */
  public static IAxis findAxisByNameNoEx( final IAxis[] axes,
      final String axisName )
  {
    try
    {
      return findAxisByName( axes, axisName );
    }
    catch( NoSuchElementException e )
    {
      return null;
    }
  }

  /**
   * Finds the axis of the given observation that has the given type.
   * 
   * @param axes
   *          the list of axes to scan
   * @param axisType
   *          the type of the axis which is searched
   * @return the first axis found
   * @throws NoSuchElementException
   *           when no axis matches the name
   */
  public static IAxis findAxisByType( final IAxis[] axes, final String axisType )
      throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].getType().equalsIgnoreCase( axisType ) )
        return axes[i];
    }

    throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + axisType );
  }

  /**
   * Returns the axes that are compatible with the desired Dataclass
   * 
   * @return all axes which are compatible with desired Classtype
   */
  public static IAxis[] findAxesByClass( final IAxis[] axes, final Class desired )
  {
    final ArrayList list = new ArrayList( axes == null ? 0 : axes.length );

    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
        list.add( axes[i] );
    }

    if( list.size() == 0 )
      throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + desired );

    return (IAxis[]) list.toArray( new IAxis[list.size()] );
  }

  /**
   * Returns the first axis that is compatible with the desired Dataclass
   */
  public static IAxis findAxisByClass( final IAxis[] axes, final Class desired )
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
        return axes[i];
    }

    throw new NoSuchElementException( MSG_ERROR_NOAXISTYPE + desired );
  }

  /**
   * Returns the first axis that is compatible with the desired Dataclass, does
   * not throw an exception if no such axis found. Returns null if no axis
   * found.
   */
  public static IAxis findAxisByClassNoEx( final IAxis[] axes,
      final Class desired )
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
        return axes[i];
    }

    return null;
  }

  /**
   * @return the axes which are key-axes. Returns an empty array if no axis
   *         found.
   */
  public static IAxis[] findAxesByKey( final IAxis[] axes )
  {
    final ArrayList list = new ArrayList( axes.length );

    for( int i = 0; i < axes.length; i++ )
    {
      if( axes[i].isKey() )
        list.add( axes[i] );
    }

    return (IAxis[]) list.toArray( new IAxis[list.size()] );
  }

  /**
   * @param sep
   *          string separator between elements
   * @return simple string representation of the given model
   */
  public static String dump( final ITuppleModel model, final String sep )
      throws SensorException
  {
    final StringWriter writer = new StringWriter();

    try
    {
      dump( model, sep, writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }

    return writer.toString();
  }

  /**
   * Dumps the contents of the model into a writer. Caller must close the
   * writer.
   */
  public static void dump( final ITuppleModel model, final String sep,
      final Writer writer ) throws SensorException
  {
    final IAxis[] axes = model.getAxisList();

    // sort axes in order to have a better output
    sortAxes( axes );

    // retrieve apropriate parsers for each axis
    final IParser[] parsers = new IParser[axes.length];
    try
    {
      for( int j = 0; j < axes.length; j++ )
        parsers[j] = ZmlFactory.createParser( axes[j] );
    }
    catch( FactoryException e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }

    try
    {
      // header
      for( int j = 0; j < axes.length; j++ )
      {
        writer.write( axes[j].toString() );

        if( j < axes.length - 1 )
          writer.write( sep );
      }

      writer.write( '\n' );

      // values
      for( int i = 0; i < model.getCount(); i++ )
      {
        // for each axis
        for( int j = 0; j < axes.length; j++ )
        {
          final IAxis axis = axes[j];

          try
          {
            writer.write( parsers[j].toString( model.getElement( i, axis ) ) );
          }
          catch( ParserException e )
          {
            e.printStackTrace();

            writer.write( "Fehler" );
          }

          if( j < axes.length - 1 )
            writer.write( sep );
        }

        writer.write( '\n' );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }

  /**
   * Dumps the tupple at given index using sep as separator.
   * 
   * @return string representation of the given line (tupple)
   */
  public static String dump( final ITuppleModel model, final String sep,
      final int index, final boolean excludeStatusAxes ) throws SensorException
  {
    IAxis[] axes = model.getAxisList();

    if( excludeStatusAxes )
      axes = KalypsoStatusUtils.withoutStatusAxes( axes );
    
    // sort axes in order to have a better output
    sortAxes( axes );

    // retrieve apropriate parsers for each axis
    final IParser[] parsers = new IParser[axes.length];
    try
    {
      for( int j = 0; j < axes.length; j++ )
        parsers[j] = ZmlFactory.createParser( axes[j] );
    }
    catch( FactoryException e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }

    final StringBuffer sb = new StringBuffer();

    for( int i = 0; i < axes.length; i++ )
    {
      try
      {
        sb.append( parsers[i].toString( model.getElement( index, axes[i] ) ) );
      }
      catch( ParserException e )
      {
        e.printStackTrace();

        sb.append( "Fehler" );
      }

      if( i < axes.length - 1 )
        sb.append( sep );
    }

    return sb.toString();
  }

  /**
   * Copy the values from source into dest. Only copies the values of the axes
   * that are found in the dest AND in source observation.
   * 
   * @param source
   *          source observation from which values are read
   * @param dest
   *          destination observation into which values are copied
   * @param args
   *          [optional, can be null] variable arguments
   * @param fullCompatibilityExpected
   *          when true an InvalidStateException is thrown to indicate that the
   *          full compatibility cannot be guaranteed. The full compatibility is
   *          expressed in terms of the axes: the source observation must have
   *          the same axes as the dest observation. If false, just the axes
   *          from dest that where found in source are used, thus leading to
   *          potential null values in the tupple model
   * @return model if some values have been copied, null otherwise
   * @throws SensorException
   * @throws IllegalStateException
   *           when compatibility is wished but could not be guaranteed
   */
  public static ITuppleModel optimisticValuesCopy( final IObservation source,
      final IObservation dest, final IVariableArguments args,
      boolean fullCompatibilityExpected ) throws SensorException,
      IllegalStateException
  {
    final IAxis[] srcAxes = source.getAxisList();
    final IAxis[] destAxes = dest.getAxisList();

    final ITuppleModel values = source.getValues( args );
    if( values == null )
      return null;

    final Map map = new HashMap();
    for( int i = 0; i < destAxes.length; i++ )
    {
      try
      {
        final IAxis A = ObservationUtilities.findAxisByType( srcAxes,
            destAxes[i].getType() );

        map.put( destAxes[i], A );
      }
      catch( NoSuchElementException e )
      {
        if( fullCompatibilityExpected
            && !KalypsoStatusUtils.isStatusAxis( destAxes[i] ) )
          throw new IllegalStateException( "Required axis " + destAxes[i]
              + " from " + dest + " could not be found in " + source );

        // else ignored, try with next one
      }
    }

    if( map.size() == 0 || values.getCount() == 0 )
      return null;

    final SimpleTuppleModel model = new SimpleTuppleModel( destAxes );

    for( int i = 0; i < values.getCount(); i++ )
    {
      final Object[] tupple = new Object[destAxes.length];

      for( int j = 0; j < destAxes.length; j++ )
      {
        final IAxis srcAxis = (IAxis) map.get( destAxes[j] );

        if( srcAxis != null )
          tupple[model.getPositionFor( destAxes[j] )] = values.getElement( i,
              srcAxis );
        else if( KalypsoStatusUtils.isStatusAxis( destAxes[j] ) )
          tupple[model.getPositionFor( destAxes[j] )] = new Integer(
              KalypsoStati.BIT_OK );
      }

      model.addTupple( tupple );
    }

    dest.setValues( model );

    return model;
  }

  /**
   * Returns the given row. Creates a new array containing the references to the
   * values in the tuppleModel for that row and these columns
   * 
   * @param row
   *          row index for which objects will be taken
   * @param axisList
   *          columns for which objects will be taken
   * @throws SensorException
   */
  public static Object[] getElements( final ITuppleModel tuppleModel,
      final int row, final IAxis[] axisList ) throws SensorException
  {
    final Object[] result = new Object[axisList.length];
    for( int i = 0; i < axisList.length; i++ )
      result[i] = tuppleModel.getElement( row, axisList[i] );
    return result;
  }

  /**
   * Sort an array of axes according to the Kalypso convention: axes are sorted
   * based on their type information. Example:
   * <p>
   * date, Q, T, V, W, etc.
   */
  public static void sortAxes( final IAxis[] axes )
  {
    Arrays.sort( axes, AXIS_SORT_COMPARATOR );
  }
  
  /**
   * AxisSortComparator: sorts the axes according to their types
   * 
   * @author schlienger (02.06.2005)
   */
  public static class AxisSortComparator implements Comparator
  {
    /**
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare( final Object o1, final Object o2 )
    {
      final IAxis a1 = (IAxis)o1;
      final IAxis a2 = (IAxis)o2;

      return a1.getType().compareTo( a2.getType() );
    }
  }
}