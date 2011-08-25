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
package org.kalypso.model.wspm.tuhh.ui.panel.roughness;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.AbstractDatabinding;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.ui.pager.IElementPage;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.binding.SLDBinding;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractRoughnessComposite implements IElementPage
{
  private final IProfil m_profile;

  private final IComponent m_component;

  private final ProfileRoguhnessesDataModel m_model;

  private IDataBinding m_binding;

  public AbstractRoughnessComposite( final IProfil profile, final IComponent component )
  {
    m_profile = profile;
    m_component = component;

    m_model = new ProfileRoguhnessesDataModel( profile, component );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.pager.IElementPage#dispose()
   */
  @Override
  public final void dispose( )
  {
    m_binding.dispose();
  }

  protected IProfil getProfile( )
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

  protected void build( final Composite body, final FormToolkit toolkit, final String label, final String property, final IValidator validator )
  {
    toolkit.createLabel( body, label );

    final Text text = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( MessageBundle.STYLE_EDITOR_FIELD_EMPTY );

    bind( text, property, validator );
  }

  private void bind( final Text textField, final String property, final IValidator validator )
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
    group.setText( "Flow Zone Roughness" );
    toolkit.adapt( group );

    setBinding( new AbstractDatabinding( toolkit )
    {
    } );

    // TODO validators
    build( group, toolkit, "Left Flood-Plain", ProfileRoguhnessesDataModel.PROPERTY_LEFT_FLOODPLAIN, null );
    build( group, toolkit, "River Tube", ProfileRoguhnessesDataModel.PROPERTY_RIVER_TUBE, null );
    build( group, toolkit, "Right Flood-Plain", ProfileRoguhnessesDataModel.PROPERTY_RIGHT_FLOODPLAIN, null );

    final ImageHyperlink lnkRemove = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkRemove.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false, 2, 0 ) );
    lnkRemove.setText( String.format( "Remove: %s", getLabel() ) );

    lnkRemove.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        RoughnessPanelHelper.removeRoughness( getProfile(), getComponent().getId() );
      }
    } );
  }

}
