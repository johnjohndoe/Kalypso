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
package org.kalypso.model.wspm.tuhh.ui.panel.roughness.pages;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.SimpleDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.ui.pager.AbstractElementPage;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.util.roughnesses.GuessRoughessClassesRunnable;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.roughness.utils.RoughnessDataModel;
import org.kalypso.model.wspm.tuhh.ui.panel.roughness.utils.RoughnessPanelHelper;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ui.editor.styleeditor.binding.SLDBinding;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractRoughnessPage extends AbstractElementPage
{
  private final IProfile m_profile;

  private final IComponent m_component;

  private final RoughnessDataModel m_model;

  private IDataBinding m_binding;

  public AbstractRoughnessPage( final IProfile profile, final IComponent component, final String identifier )
  {
    super( identifier );

    m_profile = profile;
    m_component = component;

    m_model = new RoughnessDataModel( profile, component );
  }

  @Override
  public final void dispose( )
  {
    m_binding.dispose();
  }

  protected IProfile getProfile( )
  {
    return m_profile;
  }

  protected IComponent getComponent( )
  {
    return m_component;
  }

  protected void setBinding( final IDataBinding binding )
  {
    if( Objects.isNotNull( m_binding ) )
      m_binding.dispose();

    m_binding = binding;
  }

  protected IDataBinding getBinding( )
  {
    return m_binding;
  }

  protected RoughnessDataModel getModel( )
  {
    return m_model;
  }

  protected void build( final Composite body, final FormToolkit toolkit, final String label, final String property )
  {
    toolkit.createLabel( body, label );

    final Text text = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( Messages.getString( "AbstractRoughnessPage.0" ) ); //$NON-NLS-1$

    // TODO validate text fields

    bind( text, property );
  }

  protected void bind( final Text textField, final String property )
  {
    final ISWTObservableValue targetValue = SWTObservables.observeText( textField, SLDBinding.TEXT_DEFAULT_EVENTS );
    final IObservableValue modelValue = m_model.getObservableValue( property );

    final DataBinder binder = new DataBinder( targetValue, modelValue );

    m_binding.bindValue( binder );
  }

  protected void renderSimpleType( final Composite body, final FormToolkit toolkit )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "AbstractRoughnessPage.1" ) ); //$NON-NLS-1$
    toolkit.adapt( group );

    // TODO: probably should use DatabindingPage
    setBinding( new SimpleDataBinding( toolkit ) );

    build( group, toolkit, Messages.getString( "AbstractRoughnessPage.2" ), RoughnessDataModel.PROPERTY_LEFT_FLOODPLAIN ); //$NON-NLS-1$
    build( group, toolkit, Messages.getString( "AbstractRoughnessPage.3" ), RoughnessDataModel.PROPERTY_RIVER_TUBE ); //$NON-NLS-1$
    build( group, toolkit, Messages.getString( "AbstractRoughnessPage.4" ), RoughnessDataModel.PROPERTY_RIGHT_FLOODPLAIN ); //$NON-NLS-1$

    final ImageHyperlink lnkRemove = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkRemove.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false, 2, 0 ) );
    lnkRemove.setText( String.format( Messages.getString( "AbstractRoughnessPage.5" ), getLabel() ) ); //$NON-NLS-1$

    lnkRemove.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        if( MessageDialog.openConfirm( lnkRemove.getShell(), Messages.getString("AbstractRoughnessPage.11"), Messages.getString("AbstractRoughnessPage.12") ) ) //$NON-NLS-1$ //$NON-NLS-2$
          RoughnessPanelHelper.removeRoughness( getProfile(), getComponent().getId() );
      }
    } );

    /** additional actions */
    if( hasActions() )
    {

      final Group grActions = new Group( body, SWT.NULL );
      grActions.setLayout( new GridLayout() );
      grActions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      grActions.setText( Messages.getString( "AbstractRoughnessPage.6" ) ); //$NON-NLS-1$

      final ImageHyperlink lnk = toolkit.createImageHyperlink( grActions, SWT.NULL );
      if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( getComponent().getId() ) )
        lnk.setText( Messages.getString( "AbstractRoughnessPage.7" ) ); //$NON-NLS-1$
      else if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( getComponent().getId() ) )
        lnk.setText( Messages.getString( "AbstractRoughnessPage.8" ) ); //$NON-NLS-1$

      lnk.addHyperlinkListener( new HyperlinkAdapter()
      {
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          final boolean overwriteValues = MessageDialog.openQuestion( lnk.getShell(), Messages.getString( "AbstractRoughnessPage.9" ), Messages.getString( "AbstractRoughnessPage.10" ) ); //$NON-NLS-1$ //$NON-NLS-2$

          final GuessRoughessClassesRunnable worker = new GuessRoughessClassesRunnable( getProfile(), getComponent().getId(), overwriteValues, Double.MAX_VALUE );
          ProgressUtilities.busyCursorWhile( worker );

          final ProfileOperation operation = new ProfileOperation( "Guessing roughness classes", getProfile(), overwriteValues ); //$NON-NLS-1$
          operation.addChange( worker.getChanges() );

          new ProfileOperationJob( operation ).schedule();
        }
      } );

      toolkit.adapt( grActions );
      grActions.layout();
    }

    body.layout();
  }

  private boolean hasActions( )
  {
    if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR.equals( getComponent().getId() ) )
      return false;

    return Objects.isNotNull( getProfile().hasPointProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS ) );
  }

}
