/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class AddDeviderResolution extends AbstractProfilMarkerResolution
{
  private String m_deviderType;

  /**
   * erzeugt fehlende Trennfläche
   */
  public AddDeviderResolution( final String deviderType )
  {
    super( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.resolutions.AddDeviderResolution.0" ), null, null ); //$NON-NLS-1$
    m_deviderType = deviderType;
  }

  public AddDeviderResolution( )
  {
    this( "" );//$NON-NLS-1$

  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */

  @Override
  public boolean resolve( final IProfil profil )
  {
    if( m_deviderType == "" || profil.getPoints().length < 1 ) //$NON-NLS-1$
      throw new IllegalStateException();

    final IProfilPointMarker[] markers = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( markers.length == 2 )
    {
      createMarkers( profil, markers[0].getPoint(), markers[markers.length - 1].getPoint() );
      return true;
    }
    else if( profil.getPoints().length > 0 )
    {
      createMarkers( profil, profil.getPoint( 0 ), profil.getPoint( profil.getPoints().length - 1 ) );
      return true;
    }

    return false;
  }

  private void createMarkers( final IProfil profil, final IRecord pointLeft, final IRecord pointRight )
  {
    final ProfilOperation operation = new ProfilOperation( "Add Devider", profil, true ); //$NON-NLS-1$

    // TODO: the atomic operations are adding a column and setting values to it
    // we need api for that; the marker api is too strongly coupled into the profile

    final IProfilPointMarker m1 = profil.createPointMarker( m_deviderType, pointLeft );
    final IProfilPointMarker m2 = profil.createPointMarker( m_deviderType, pointRight );
    m1.setInterpretedValue( true );
    m2.setInterpretedValue( true );

    // operation.addChange( new PointMarkerSetPoint( devider, newPoint ) );

    operation.addChange( new ActiveObjectEdit( profil, pointLeft, null ) );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#setData(java.lang.String)
   */
  @Override
  public void setData( final String parameterStream )
  {
    final String[] params = getParameter( parameterStream );
    try
    {
      m_deviderType = params[1];
    }
    catch( final Exception e )
    {
      throw new IllegalArgumentException();
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#getSerializedParameter()
   */
  @Override
  public String getSerializedParameter( )
  {
    return super.getSerializedParameter() + ";" + m_deviderType; //$NON-NLS-1$
  }

}
