/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.client.ui.project.database;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.afgui.extension.IProjectDatabaseUiLocker;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.interfaces.IProjectDatabaseModel;
import org.kalypso.project.database.client.ui.project.database.internal.IProjectRowBuilder;
import org.kalypso.project.database.client.ui.project.database.internal.ProjectRowBuilderFabrication;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseListener;

/**
 * Composite for rendering and handling remote and local projects
 *
 * @author Dirk Kuch
 */
public class ProjectDatabaseComposite extends Composite implements IProjectDatabaseListener, IPreferenceChangeListener, IProjectDatabaseUiLocker
{
  private final FormToolkit m_toolkit;

  private Composite m_body = null;

  private final IProjectDatabaseModel m_model;

  private final IProjectDatabaseFilter m_filter;

  protected UIJob m_updateJob = null;

  private boolean m_updateLock = false;

  private final IKalypsoProjectOpenAction m_openAction;

  /**
   * @param parent
   *          composite
   * @param localProjectNatures
   *          handle project with these project nature ids TODO perhaps delegate.getProjects()
   * @param remoteProjectTypes
   *          handle remote projects with these type ids //TODO filter
   * @param isExpert
   *          show expert debug informations?
   */
  public ProjectDatabaseComposite( final Composite parent, final FormToolkit toolkit, final IProjectDatabaseFilter filter, final IKalypsoProjectOpenAction openAction )
  {
    super( parent, SWT.NONE );
    m_toolkit = toolkit;
    m_filter = filter;
    m_openAction = openAction;

    m_model = KalypsoProjectDatabaseClient.getDefault().getProjectDatabaseModel();
    m_model.addListener( this );

    update();
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    m_model.removeListener( this );
  }

  /**
   * @see org.eclipse.swt.widgets.Control#update()
   */
  @Override
  public final void update( )
  {
    if( m_updateLock )
      return;

    if( this.isDisposed() )
      return;

    if( m_body != null && !m_body.isDisposed() )
    {
      m_body.dispose();
      m_body = null;
    }

    m_body = m_toolkit.createComposite( this );
    m_body.setLayout( new GridLayout() );
    m_body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final IProjectHandler[] projects = m_model.getProjects( m_filter );
    for( final IProjectHandler project : projects )
    {
      final IProjectRowBuilder builder = ProjectRowBuilderFabrication.getBuilder( project, m_openAction, this );
      builder.render( m_body, m_toolkit );
    }

    m_toolkit.adapt( this );
    this.layout();
  }

  /**
   * @see org.kalypso.project.database.client.core.interfaces.IProjectDatabaseListener#projectModelChanged()
   */
  @Override
  public void projectModelChanged( )
  {
    updateUI();
  }

  /**
   * @see org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener#preferenceChange(org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent)
   */
  @Override
  public void preferenceChange( final PreferenceChangeEvent event )
  {
    updateUI();
  }

  private void updateUI( )
  {
    if( m_updateLock )
      return;

    if( m_updateJob == null )
    {
      m_updateJob = new UIJob( "" )
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          update();
          m_updateJob = null;

          return Status.OK_STATUS;
        }
      };

      m_updateJob.schedule( 750 );
    }
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.list.IProjectDatabaseUIHandler#acquireUiUpdateLock()
   */
  @Override
  public void acquireUiUpdateLock( )
  {
    m_updateLock = true;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.list.IProjectDatabaseUIHandler#releaseUiUpdateLock()
   */
  @Override
  public void releaseUiUpdateLock( )
  {
    m_updateLock = false;
    updateUI();
  }
}
