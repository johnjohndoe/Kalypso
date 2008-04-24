/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.ui.profil.dialogs.reducepoints;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.observation.result.IRecord;

/**
 * Delivers points from a selection.
 * 
 * @author Belger
 */
public class SelectionPointsProvider implements IPointsProvider
{
  private final IProfil m_profil;

  private String m_errorMessage;

  private IRecord[] m_points = new IRecord[] {};

  public SelectionPointsProvider( final IProfil profil, final ISelection selection )
  {
    m_profil = profil;

    if( selection.isEmpty() )
    {
      m_errorMessage = Messages.SelectionPointsProvider_0;
      return;
    }

    if( selection instanceof IStructuredSelection )
    {
// try
// {
      final IStructuredSelection structSel = (IStructuredSelection) selection;

      if( structSel.size() < 3 )
      {
        m_errorMessage = Messages.SelectionPointsProvider_1;
        return;
      }

      final Object[] objects = structSel.toArray();

      IRecord lastPoint = null;
      final List<IRecord> points = new ArrayList<IRecord>( objects.length );
      for( final Object object : objects )
      {
        if( object instanceof IRecord )
        {
          final IRecord point = (IRecord) object;
          if( lastPoint != null && lastPoint != ProfilUtil.getPointBefore( m_profil, point ) )
          {
            m_errorMessage = Messages.SelectionPointsProvider_2;
            return;
          }

          points.add( point );
          lastPoint = point;
        }
        else
        {
          m_errorMessage = Messages.SelectionPointsProvider_3;
          return;
        }
      }

      m_errorMessage = null;
      m_points = points.toArray( new IRecord[points.size()] );
      return;
// }
// catch( final IllegalProfileOperationException e )
// {
// m_errorMessage = "Fehler beim Prüfen der Selektion: " + e.getLocalizedMessage();
// return;
// }
    }

    m_errorMessage = Messages.SelectionPointsProvider_4;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.douglasPeucker.IPointsProvider#getPoints()
   */
  public IRecord[] getPoints( )
  {
    return m_points;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.douglasPeucker.IPointsProvider#getErrorMessage()
   */
  public String getErrorMessage( )
  {
    return m_errorMessage;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.douglasPeucker.IPointsProvider#getName()
   */
  public String getName( )
  {
    return Messages.SelectionPointsProvider_5;
  }
}
