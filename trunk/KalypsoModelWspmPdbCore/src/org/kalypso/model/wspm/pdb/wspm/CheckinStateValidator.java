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
package org.kalypso.model.wspm.pdb.wspm;

import java.util.Set;

import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.databinding.validation.TypedValidator;

/**
 * Specialized validator that handles update of existing states.
 *
 * @author Gernot Belger
 */
class CheckinStateValidator extends TypedValidator<String>
{
  private static final String STR_UPLOAD_IMPOSSIBLE = "Bitte geben Sie einen Namen für einen neuen Zustand ein.";

  private final Set<String> m_allStateNames;

  private final Set<String> m_updateableStateNames;

  private final Set<String> m_notUpdateableStateNames;

  public CheckinStateValidator( final Set<String> allStateNames, final Set<String> notUpdateableNames, final Set<String> updateableNames )
  {
    super( String.class, IStatus.ERROR, "Es existiert bereits ein Zustand mit diesem Namen in einem anderen Gewässer.\n" + STR_UPLOAD_IMPOSSIBLE );

    m_allStateNames = allStateNames;
    m_notUpdateableStateNames = notUpdateableNames;
    m_updateableStateNames = updateableNames;
  }

  @Override
  protected IStatus doValidate( final String value ) throws CoreException
  {
    if( m_notUpdateableStateNames.contains( value ) )
      return ValidationStatus.error( "Ein Zustand mit gleichem Namen existiert bereits, ist aber schreibgeschützt.\n" + STR_UPLOAD_IMPOSSIBLE );

    if( m_updateableStateNames.contains( value ) )
      return ValidationStatus.warning( "Ein Zustand mit gleichem Namen existiert bereits.\nDer Zustand wird überschrieben." );

    if( m_allStateNames.contains( value ) )
      fail();

    return ValidationStatus.ok();
  }
}