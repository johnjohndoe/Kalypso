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
package org.kalypso.ui.wizards.results;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.action.AddThemeCommand;

/**
 * Adds results as themes to the map.
 * 
 * @author Gernot Belger
 */
class AddResultThemeOperation implements ICoreRunnableWithProgress
{
  private final IKalypsoLayerModell m_model;

  private final ICommandTarget m_commandTarget;

  private final IResultMeta[] m_results;

  private final ThemeConstructionFactory m_factory;

  public AddResultThemeOperation( final IKalypsoLayerModell model, final ICommandTarget commandTarget, final IResultMeta[] results, final ThemeConstructionFactory factory )
  {
    m_model = model;
    m_commandTarget = commandTarget;
    m_results = results;
    m_factory = factory;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.MapUtils.5" ), m_results.length ); //$NON-NLS-1$

    for( final IResultMeta resultMeta : m_results )
    {
      final AbstractThemeCreator themeCreator = m_factory.createThemeConstructor( resultMeta );
      final ResultAddLayerCommandData[] datas = themeCreator.getThemeCommandData();
      if( datas != null )
      {
        for( final ResultAddLayerCommandData data : datas )
        {
          if( m_model != null )
          {
            final AddThemeCommand addThemeCommand = new AddThemeCommand( m_model, data.getThemeName(), data.getResultType(), data.getFeaturePath(), data.getSource() );
            addThemeCommand.addStyle( data.getStyle(), data.getStyleLocation() );
            addThemeCommand.addProperties( data.getProperties() );
            m_commandTarget.postCommand( addThemeCommand, null );
          }
        }
      }

      // TODO:
      // - create sub-themes for container results (also use filter for children)
      // - ...

      monitor.worked( 1 );
      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;
    }

    return Status.OK_STATUS;
  }
}
