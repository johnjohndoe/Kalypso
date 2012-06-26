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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.cm.view.action.DeleteGeneratorAction;
import org.kalypso.ui.rrm.internal.cm.view.action.NewLinearSumGeneratorAction;
import org.kalypso.ui.rrm.internal.cm.view.action.NewLinearSumIdwAction;
import org.kalypso.ui.rrm.internal.cm.view.action.NewLinearSumThiessenAction;
import org.kalypso.ui.rrm.internal.cm.view.action.NewMultiGeneratorAction;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.ParameterComposite;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class ParameterGeneratorUiHandler extends AbstractTreeNodeUiHandler
{
  private final ITreeNodeModel m_model;

  private final String m_parameterType;

  private final IRainfallGenerator[] m_generators;

  public ParameterGeneratorUiHandler( final ITreeNodeModel model, final String parameterType, final IRainfallGenerator[] generators )
  {
    m_model = model;
    m_parameterType = parameterType;
    m_generators = generators;
  }

  @Override
  public String getTypeLabel( )
  {
    return Messages.getString( "ParameterGeneratorUiHandler_0" ); //$NON-NLS-1$
  }

  @Override
  public String getTreeLabel( )
  {
    return Messages.getString( "ParameterGeneratorUiHandler.0", ParameterTypeUtils.formatParameterType( m_parameterType ) ); //$NON-NLS-1$
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    final String imageLocation = UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_BASE.getImagePath() + "_" + m_parameterType + ".png"; //$NON-NLS-1$ //$NON-NLS-2$
    return UIRrmImages.id( imageLocation );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return new ParameterComposite( parent, binding, m_parameterType );
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewLinearSumGeneratorAction( m_model, m_parameterType ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewLinearSumThiessenAction( m_model, m_parameterType ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewLinearSumIdwAction( m_model, m_parameterType ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewMultiGeneratorAction( m_model ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new DeleteGeneratorAction( m_generators ) );
  }
}