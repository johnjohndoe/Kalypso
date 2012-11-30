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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.databinding.validation.TypedValidator;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class UniqueSettingsNameValidator extends TypedValidator<String>
{
  private final Set<String> m_names = new HashSet<>();

  private final String m_ignoreName;

  /**
   * @param ignoreName
   *          Names that are in the list of existing settings but have this name should be ignored.
   */
  public UniqueSettingsNameValidator( final String ignoreName )
  {
    super( String.class, IStatus.ERROR, Messages.getString( "UniqueSettingsNameValidator.0" ) ); //$NON-NLS-1$

    m_ignoreName = ignoreName;

    try
    {
      final IPdbSettings[] settings = PdbSettings.getSettings();
      for( final IPdbSettings s : settings )
        m_names.add( s.getName() );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
    }
  }

  @Override
  protected IStatus doValidate( final String value ) throws CoreException
  {
    if( value.equals( m_ignoreName ) )
      return Status.OK_STATUS;

    if( m_names.contains( value ) )
      fail();

    return Status.OK_STATUS;
  }
}