/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WaterlevelObject
{
  /** specialized metadata on waterlevel object to transport label to layer */
  static final String METADATA_LABEL = "waterlevelLabel"; //$NON-NLS-1$

  private final Map<String, Collection<IProfileObject>> m_objects = new HashMap<>();

  private final String m_label;

  private final String m_eventType;

  public WaterlevelObject( final String label, final String eventType )
  {
    m_label = label;
    m_eventType = eventType;
  }

  public void addObject( final IProfileObject object )
  {
    final String type = object.getType();
    switch( type )
    {
      case IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS:
      case IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT:

        object.setValue( METADATA_LABEL, getLabel() );

        final Collection<IProfileObject> objects = getObjectList( type );
        objects.add( object );

        return;

      default:
        throw new IllegalArgumentException();
    }
  }

  private Collection<IProfileObject> getObjectList( final String type )
  {
    if( !m_objects.containsKey( type ) )
      m_objects.put( type, new ArrayList<IProfileObject>() );

    return m_objects.get( type );
  }

  public IProfileObject[] getObjects( final String type )
  {
    final Collection<IProfileObject> objects = getObjectList( type );
    return objects.toArray( new IProfileObject[objects.size()] );
  }

  public boolean isEmpty( )
  {
    for( final String key : m_objects.keySet() )
    {
      if( !m_objects.get( key ).isEmpty() )
        return false;
    }

    return true;
  }

  public String getLabel( )
  {
    if( StringUtils.isBlank( m_label ) )
      return Messages.getString("WaterlevelObject_0"); //$NON-NLS-1$

    return m_label;
  }

  public String getType( )
  {
    return m_eventType;
  }
}