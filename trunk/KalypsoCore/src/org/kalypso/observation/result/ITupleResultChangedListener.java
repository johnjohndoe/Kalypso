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
package org.kalypso.observation.result;

/**
 * Listener which will be informed of changes of a {@link org.kalypso.observation.result.TupleResult}.
 * 
 * @author Gernot Belger
 */
public interface ITupleResultChangedListener
{
  public static class ValueChange
  {
    private final IRecord m_record;

    private final IComponent m_component;

    private final Object m_newValue;

    public ValueChange( final IRecord record, final IComponent component, final Object newValue )
    {
      m_record = record;
      m_component = component;
      m_newValue = newValue;
    }

    public IComponent getComponent( )
    {
      return m_component;
    }

    public Object getNewValue( )
    {
      return m_newValue;
    }

    public IRecord getRecord( )
    {
      return m_record;
    }
  }

  public enum TYPE
  {
    ADDED,
    REMOVED,
    CHANGED;
  }

  public void valuesChanged( final ValueChange[] changes );

  public void recordsChanged( final IRecord[] records, final TYPE type );

  public void componentsChanged( final IComponent[] components, final TYPE type );
}
