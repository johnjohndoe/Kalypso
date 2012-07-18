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
package org.kalypso.ui.rrm.internal.simulations;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IInputValidator;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Holger Albert
 */
public class SimulationDescriptionValidator implements IInputValidator
{
  private final NAControl m_simulation;

  public SimulationDescriptionValidator( final NAControl simulation )
  {
    m_simulation = simulation;
  }

  @Override
  public String isValid( final String newText )
  {
    if( newText == null || newText.length() == 0 )
      return Messages.getString("SimulationDescriptionValidator.0"); //$NON-NLS-1$

    final NAControl[] allSimulations = getAllSimulations();
    final String duplicate = findDuplicates( allSimulations, newText );
    if( duplicate != null )
      return String.format( Messages.getString("SimulationDescriptionValidator.1"), newText ); //$NON-NLS-1$

    final String validatedText = FileUtilities.validateName( newText, "_" ); //$NON-NLS-1$
    if( !newText.equals( validatedText ) )
      return String.format( Messages.getString("SimulationDescriptionValidator.3"), newText ); //$NON-NLS-1$

    return null;
  }

  private NAControl[] getAllSimulations( )
  {
    final SimulationCollection owner = (SimulationCollection) m_simulation.getOwner();
    final IFeatureBindingCollection<NAControl> allSimulations = owner.getSimulations();
    return allSimulations.toArray( new NAControl[] {} );
  }

  /**
   * This function checks all simulations for duplicates.
   * 
   * @param allSimulations
   *          All simulations.
   * @return The first name of the simulation, which is a duplicate or null.
   */
  private String findDuplicates( final NAControl[] allSimulations, final String currentText )
  {
    final Set<String> allNames = new HashSet<>();

    for( final NAControl simulation : allSimulations )
    {
      final String name = simulation.getDescription();
      allNames.add( name );
    }

    if( allNames.contains( currentText ) )
      return currentText;

    return null;
  }
}