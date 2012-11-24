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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class ProfileResultExportPage extends ValidatingWizardPage
{
  private static final String STR_AVAILABLE_RESULTS = Messages.getString( "ProfileResultExportPage_0" ); //$NON-NLS-1$

  private final ProfileExportResultChooser m_resultChooser;

  private ProfileExportComponentChooser m_componentChooser;

  private boolean m_showComponentChooser = true;

  /**
   * Same as {@link #ProfileResultExportPage(String, IWspmResultNode, false)}
   */
  public ProfileResultExportPage( final String pageName, final IWspmResultNode results )
  {
    this( pageName, results, false );
  }

  /**
   * @param singleSelection
   *          If <code>true</code>, only one element can be selected.
   */
  public ProfileResultExportPage( final String pageName, final IWspmResultNode results, final boolean singleSelection )
  {
    super( pageName );

    setTitle( STR_AVAILABLE_RESULTS );
    setDescription( Messages.getString( "ProfileResultExportPage_1" ) ); //$NON-NLS-1$

    final ViewerFilter filter = new EmptyWaterlevelFilter();

    m_resultChooser = new ProfileExportResultChooser( results, singleSelection, filter );
    m_resultChooser.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateMessage();
      }
    } );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite resultGroup = createResultGroup( parent );
    setControl( resultGroup );

    super.createControl( parent );
  }

  private Composite createResultGroup( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    final GridLayout layout = new GridLayout( 1, false );
    group.setLayout( layout );
    group.setText( STR_AVAILABLE_RESULTS );

    final SashForm sashForm = new SashForm( group, SWT.HORIZONTAL );
    sashForm.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_componentChooser = createComponentChooser();

    m_resultChooser.createControl( sashForm );

    if( m_componentChooser != null )
    {
      m_componentChooser.createControl( sashForm );
      sashForm.setWeights( new int[] { 50, 50 } );
    }
    else
    {
      sashForm.setWeights( new int[] { 100 } );
    }

    return group;
  }

  private ProfileExportComponentChooser createComponentChooser( )
  {
    if( !m_showComponentChooser )
      return null;

    final ProfileExportComponentChooser componentChooser = new ProfileExportComponentChooser( getDialogSettings() );
    componentChooser.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateMessage();
      }
    } );

    return componentChooser;
  }

  public WspmResultLengthSectionColumn[] getSelectedColumns( )
  {
    if( m_componentChooser == null )
      return null;

    final Collection<WspmResultLengthSectionColumn> columns = new ArrayList<>();

    final IComponent[] components = m_componentChooser.getSelectedComponents();

    final WspmResultLengthSection[] lengthSections = getSelectedLengthSections();
    for( final WspmResultLengthSection section : lengthSections )
    {
      for( final IComponent component : components )
      {
        if( section.hasColumn( component ) )
        {
          final WspmResultLengthSectionColumn column = section.getColumn( component );
          columns.add( column );
        }
      }
    }

    return columns.toArray( new WspmResultLengthSectionColumn[columns.size()] );
  }

  public WspmResultLengthSection[] getSelectedLengthSections( )
  {
    final IWspmResult[] results = getSelectedResults();
    final WspmResultLengthSection[] sections = new WspmResultLengthSection[results.length];
    for( int i = 0; i < results.length; i++ )
    {
      sections[i] = results[i].getLengthSection();
    }

    return sections;
  }

  public IWspmResult[] getSelectedResults( )
  {
    final Collection<IWspmResult> lengthSections = new ArrayList<>();

    final IWspmResultNode[] results = m_resultChooser.getSelectedResults();
    for( final IWspmResultNode result : results )
    {
      if( result instanceof IWspmResult )
      {
        lengthSections.add( (IWspmResult)result );
      }
    }

    return lengthSections.toArray( new IWspmResult[lengthSections.size()] );
  }

  @Override
  protected IMessageProvider validatePage( )
  {
    if( ArrayUtils.isEmpty( getSelectedResults() ) )
      return new MessageProvider( Messages.getString( "ProfileResultExportPage_2" ), INFORMATION ); //$NON-NLS-1$

    return null;
  }

  public void setShowComponentChooser( final boolean showComponentChooser )
  {
    m_showComponentChooser = showComponentChooser;
  }
}
