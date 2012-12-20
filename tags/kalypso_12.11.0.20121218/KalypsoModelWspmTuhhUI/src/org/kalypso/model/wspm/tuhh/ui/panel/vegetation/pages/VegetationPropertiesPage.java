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
package org.kalypso.model.wspm.tuhh.ui.panel.vegetation.pages;

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
import org.kalypso.contribs.eclipse.ui.pager.AbstractElementPage;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.util.vegetation.GuessVegetationClassesRunnable;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.roughness.utils.RoughnessDataModel;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils.VegetationPanelHelper;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils.VegetationsDataModel;
import org.kalypso.ui.editor.styleeditor.binding.SLDBinding;

/**
 * @author Dirk Kuch
 */
public class VegetationPropertiesPage extends AbstractElementPage
{
  private static final String EMPTY_STRING = Messages.getString( "VegetationPropertiesPage.0" ); //$NON-NLS-1$

  protected final IProfile m_profile;

  private IDataBinding m_binding;

  public VegetationPropertiesPage( final IProfile profile )
  {
    super( VegetationPropertiesPage.class.getName() );
    m_profile = profile;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "VegetationPropertiesPage.1" ); //$NON-NLS-1$
  }

  @Override
  public void render( final Composite body, final FormToolkit toolkit )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( new GridLayout( 4, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "VegetationPropertiesPage.2" ) ); //$NON-NLS-1$
    toolkit.adapt( group );

    // FIXME: probably should use databinding wizard page
    m_binding = new SimpleDataBinding( toolkit );

    toolkit.createLabel( group, StringUtils.EMPTY );
    toolkit.createLabel( group, Messages.getString( "VegetationPropertiesPage.4" ) ).setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) ); //$NON-NLS-1$
    toolkit.createLabel( group, Messages.getString( "VegetationPropertiesPage.5" ) ).setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) ); //$NON-NLS-1$
    toolkit.createLabel( group, Messages.getString( "VegetationPropertiesPage.6" ) ).setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) ); //$NON-NLS-1$

    build( group, toolkit, Messages.getString( "VegetationPropertiesPage.7" ), RoughnessDataModel.PROPERTY_LEFT_FLOODPLAIN ); //$NON-NLS-1$
    build( group, toolkit, Messages.getString( "VegetationPropertiesPage.9" ), RoughnessDataModel.PROPERTY_RIGHT_FLOODPLAIN ); //$NON-NLS-1$

    final ImageHyperlink lnkRemove = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkRemove.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false, 4, 0 ) );
    lnkRemove.setText( String.format( Messages.getString( "VegetationPropertiesPage.10" ), getLabel() ) ); //$NON-NLS-1$

    lnkRemove.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        if( MessageDialog.openConfirm( lnkRemove.getShell(), Messages.getString("VegetationPropertiesPage.3"), Messages.getString("VegetationPropertiesPage.15") ) ) //$NON-NLS-1$ //$NON-NLS-2$
          VegetationPanelHelper.removeVegetationTypes( m_profile );
      }
    } );

    /** additional actions */
    if( hasActions() )
    {

      final Group grActions = new Group( body, SWT.NULL );
      grActions.setLayout( new GridLayout() );
      grActions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      grActions.setText( Messages.getString( "VegetationPropertiesPage.11" ) ); //$NON-NLS-1$

      final ImageHyperlink lnk = toolkit.createImageHyperlink( grActions, SWT.NULL );
      lnk.setText( Messages.getString( "VegetationPropertiesPage.12" ) ); //$NON-NLS-1$
      lnk.addHyperlinkListener( new HyperlinkAdapter()
      {
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          final boolean overwriteValues = MessageDialog.openQuestion( lnk.getShell(), Messages.getString( "VegetationPropertiesPage.13" ), Messages.getString( "VegetationPropertiesPage.14" ) ); //$NON-NLS-1$ //$NON-NLS-2$

          final GuessVegetationClassesRunnable worker = new GuessVegetationClassesRunnable( m_profile, overwriteValues, Double.MAX_VALUE );
          ProgressUtilities.busyCursorWhile( worker );

          final ProfileOperation operation = new ProfileOperation( "Guessing vegetation classes", m_profile, overwriteValues ); //$NON-NLS-1$
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
    return WspmClassifications.hasVegetationClass( m_profile );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.pager.IElementPage#dispose()
   */
  @Override
  public void dispose( )
  {
  }

  protected void build( final Composite body, final FormToolkit toolkit, final String label, final String property )
  {
    toolkit.createLabel( body, label );

    final VegetationsDataModel modelAx = new VegetationsDataModel( m_profile, m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) );
    final VegetationsDataModel modelAy = new VegetationsDataModel( m_profile, m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ) );
    final VegetationsDataModel modelDp = new VegetationsDataModel( m_profile, m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ) );

    final Text ax = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    ax.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    ax.setMessage( EMPTY_STRING );

    bind( ax, modelAx, property );

    final Text ay = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    ay.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    ay.setMessage( EMPTY_STRING );

    bind( ay, modelAy, property );

    final Text dp = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    dp.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    dp.setMessage( EMPTY_STRING );

    bind( dp, modelDp, property );
  }

  protected void bind( final Text text, final VegetationsDataModel model, final String property )
  {
    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SLDBinding.TEXT_DEFAULT_EVENTS );
    final IObservableValue modelValue = model.getObservableValue( property );

    final DataBinder binder = new DataBinder( targetValue, modelValue );

    m_binding.bindValue( binder );
  }

}
