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
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.core.databinding.property.INativePropertyListener;
import org.eclipse.core.databinding.property.ISimplePropertyListener;
import org.eclipse.core.databinding.property.value.SimpleValueProperty;
import org.eclipse.core.resources.IFolder;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.ResultInfoBuilder;

/**
 * @author Gernot Belger
 */
class RestartSorterLabelProvider extends SimpleValueProperty
{
  private final ResultInfoBuilder m_infoBuilder = new ResultInfoBuilder();

  private final IFolder m_currentScenario;

  public RestartSorterLabelProvider( final IFolder currentScenario )
  {
    m_currentScenario = currentScenario;
  }

  @Override
  protected Object doGetValue( final Object source )
  {
    return m_infoBuilder.formatResultLabel( (IResultMeta)source, m_currentScenario );
  }

  @Override
  protected void doSetValue( final Object source, final Object value )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public INativePropertyListener adaptListener( final ISimplePropertyListener listener )
  {
    return null;
  }

  @Override
  public Object getValueType( )
  {
    return String.class;
  }
}