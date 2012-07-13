/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.timeseries.view.dnd;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

/**
 * @author Dirk Kuch
 */
public class MoveStationTransfer extends ByteArrayTransfer
{
  private final Cache<String, ITimeseries> m_cache;

  private static final String TYPE_NAME = "moveStationTransfer"; //$NON-NLS-1$

  private static final int TYPEID = registerType( TYPE_NAME );

  private static final MoveStationTransfer INSTANCE = new MoveStationTransfer();

  public MoveStationTransfer( )
  {
    m_cache = CacheBuilder.newBuilder().expireAfterWrite( 2, TimeUnit.MINUTES ).build();
  }

  @Override
  protected int[] getTypeIds( )
  {
    return new int[] { TYPEID };
  }

  @Override
  protected String[] getTypeNames( )
  {
    return new String[] { TYPE_NAME };
  }

  public static Transfer getInstance( )
  {
    return INSTANCE;
  }

  /*
   * Method declared on Transfer.
   */
  @Override
  protected void javaToNative( final Object object, final TransferData transferData )
  {
    if( object == null )
      return;

    m_cache.cleanUp(); // clean old obsolete entries

    final ITimeseries[] timeserieses = (ITimeseries[]) object;
    final String[] identifiers = new String[timeserieses.length];

    for( int i = 0; i < timeserieses.length; i++ )
    {
      final ITimeseries timeseries = timeserieses[i];
      final String identifier = timeseries.getId();

      identifiers[i] = identifier;

      m_cache.put( identifier, timeseries );
    }

    final String transfer = Joiner.on( ';' ).join( identifiers ); //$NON-NLS-1$

    final byte[] bytes = transfer.getBytes();

    super.javaToNative( bytes, transferData );
  }

  /*
   * Method declared on Transfer.
   */
  @Override
  protected Object nativeToJava( final TransferData transferData )
  {
    final byte[] bytes = (byte[]) super.nativeToJava( transferData );
    final String transfer = new String( bytes );

    final Set<ITimeseries> timeserieses = new LinkedHashSet<>();

    final Iterable<String> identifiers = Splitter.on( ';' ).split( transfer ); //$NON-NLS-1$
    for( final String identifier : identifiers )
    {
      final ITimeseries timeseries = m_cache.getIfPresent( identifier );
      if( Objects.isNotNull( timeseries ) )
        timeserieses.add( timeseries );
    }

    return timeserieses.toArray( new ITimeseries[] {} );
  }
}