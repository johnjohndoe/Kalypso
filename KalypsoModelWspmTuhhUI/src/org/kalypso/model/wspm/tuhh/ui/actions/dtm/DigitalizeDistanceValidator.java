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
package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.eclipse.jface.dialogs.IInputValidator;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class DigitalizeDistanceValidator implements IInputValidator
{
  private final double MIN_TOLERANCE = 0.01;

  @Override
  public String isValid( final String newText )
  {
    try
    {
      final double value = NumberUtils.parseDouble( newText );
      if( value < MIN_TOLERANCE )
        return String.format( Messages.getString("DigitalizeDistanceValidator_0"), MIN_TOLERANCE ); //$NON-NLS-1$
    }
    catch( final NumberFormatException e )
    {
      return Messages.getString("DigitalizeDistanceValidator_1"); //$NON-NLS-1$
    }

    return null;
  }
}