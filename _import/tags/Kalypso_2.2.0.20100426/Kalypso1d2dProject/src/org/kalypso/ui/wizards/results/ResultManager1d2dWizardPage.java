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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.wizards.i18n.Messages;

/**
 * @author Thomas Jung
 * 
 */
public class ResultManager1d2dWizardPage extends SelectResultWizardPage
{

  protected IKalypsoLayerModell m_modell;

  protected ICommandTarget m_commandTarget;

  public ResultManager1d2dWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final ViewerFilter filter, Result1d2dMetaComparator comparator, final IThemeConstructionFactory factory )
  {
    super( pageName, title, titleImage, filter, comparator, factory );
  }

  /**
   * @see org.kalypso.ui.wizards.results.SelectResultWizardPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    // HACK: add an extra button 'Delete'
    final Composite panel = new Composite( parent, SWT.BORDER );
    panel.setLayout( new GridLayout() );

    super.createControl( panel );

    getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Button deleteButton = new Button( panel, SWT.PUSH );
    deleteButton.setToolTipText( Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.0") ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    final Image deleteImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.DELETE );
    deleteButton.setImage( deleteImage );
    deleteButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final CheckboxTreeViewer treeViewer = getTreeViewer();
        final IResultMeta[] selectedResults = getSelectedResults();
        for( final IResultMeta resultMeta : selectedResults )
        {
          if( resultMeta instanceof IStepResultMeta )
          {

            /* handle result meta entries */
            final IStepResultMeta stepResult = (IStepResultMeta) resultMeta;
            ResultMeta1d2dHelper.removeResult( stepResult );

            /* handle map themes */
            if( m_modell != null && m_commandTarget != null )
              ResultMeta1d2dHelper.deleteResultThemeFromMap( stepResult, m_modell, m_commandTarget );

            /* handle tree */
            ViewerUtilities.refresh( treeViewer, true );
          }
        }
      }
    } );

    setControl( panel );
  }

  public void setMapModel( IKalypsoLayerModell modell )
  {
    m_modell = modell;
  }

  public void setCommandTarget( ICommandTarget commandTarget )
  {
    m_commandTarget = commandTarget;
  }

}
