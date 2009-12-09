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
package org.kalypso.model.wspm.tuhh.core.profile.importer.hw;

import java.util.Formatter;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;

import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * @author belger
 */
public class ProblemResult implements IHeightWidthResult
{
  protected final static GeometryFactory GF = new GeometryFactory();

  private Status m_status;

  private final String m_name;

  public ProblemResult( final String name, final Status status )
  {
    m_name = name;
    m_status = status;
  }

  public String getName( )
  {
    return m_name;
  }

  protected void addStatus( final int severity, final String message, final Throwable exception )
  {
    if( m_status == null )
      m_status = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhCorePlugin.getDefault() ), -1, "", null ); //$NON-NLS-1$
    else if( !(m_status instanceof MultiStatus) )
      m_status = new MultiStatus( m_status.getPlugin(), m_status.getCode(), m_status.getMessage(), m_status.getException() );

    ((MultiStatus) m_status).add( new Status( severity, PluginUtilities.id( KalypsoModelWspmTuhhCorePlugin.getDefault() ), message, exception ) );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.hw.IHeightWidthResult#formatOut(java.util.Formatter)
   */
  @Override
  public void formatOut( final Formatter formatter )
  {
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.hw.IHeightWidthResult#format(java.util.Formatter)
   */
  @Override
  public void formatLog( final Formatter formatter )
  {
    formatter.format( "%s%n%n", m_name ); //$NON-NLS-1$

    if( m_status == null || m_status.isOK() )
      return;

    formatter.format( "Warnings while processing:%n" ); //$NON-NLS-1$

    formatStatus( formatter, m_status, 0 );
    formatter.format( "%n" ); //$NON-NLS-1$
  }

  private void formatStatus( final Formatter formatter, final IStatus status, final int indent )
  {
    final int indentation = (indent + 1) * 2;
    final String formatString = "%" + indentation + "s%s: %s (%s)%n"; //$NON-NLS-1$ //$NON-NLS-2$
    final String severity = StatusUtilities.getLocalizedSeverity( status );
    formatter.format( formatString, "", severity, status.getMessage(), status.getException() ); //$NON-NLS-1$
    final IStatus[] children = status.getChildren();
    for( final IStatus child : children )
      formatStatus( formatter, child, indent + 1 );
  }

}
