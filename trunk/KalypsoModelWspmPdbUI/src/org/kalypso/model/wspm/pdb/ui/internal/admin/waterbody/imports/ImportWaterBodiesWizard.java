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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.ui.wizard.shape.SelectShapeFilePage;

/**
 * @author Gernot Belger
 */
public class ImportWaterBodiesWizard extends Wizard
{
  private final IPageChangedListener m_pageListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChange( (IWizardPage) event.getSelectedPage() );
    }
  };

  private final SelectShapeFilePage m_shapeFilePage;

  private final ImportWaterBodiesData m_data;

  public ImportWaterBodiesWizard( final ImportWaterBodiesData data )
  {
    m_data = data;

    setWindowTitle( "Import Water Bodies" );
    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );
    setNeedsProgressMonitor( true );

    m_shapeFilePage = new SelectShapeFilePage( "selectPage", "Select Shape File", WspmPdbUiImages.IMG_WIZBAN_IMPORT_WIZ ); //$NON-NLS-1$
    m_shapeFilePage.setDescription( "Select the shape file of river lines on this page." );
    addPage( m_shapeFilePage );

    addPage( new ImportWaterbodiesSelectAttributesPage( "selectAttributes", data ) ); //$NON-NLS-1$
    addPage( new ImportWaterbodiesPreviewPage( "previewPage", data ) ); //$NON-NLS-1$
  }

  @Override
  public boolean canFinish( )
  {
    final boolean canFinish = super.canFinish();
    final boolean hasNextPage = getNextPage( getContainer().getCurrentPage() ) != null;

    return canFinish && !hasNextPage;
  }

  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) wizardContainer).addPageChangedListener( m_pageListener );
  }

  protected void handlePageChange( final IWizardPage page )
  {
    final String shapeFile = m_shapeFilePage.getShapeFile();
    final String srs = m_shapeFilePage.getSoureCRS();
    m_data.setShapeInput( shapeFile, srs );

    if( page instanceof IUpdateable )
      ((IUpdateable) page).update();
  }

  @Override
  public boolean performFinish( )
  {
    final WritableSet selectedWaterBodies = m_data.getSelectedWaterBodies();
    final WaterBody[] waterBodies = (WaterBody[]) selectedWaterBodies.toArray( new WaterBody[selectedWaterBodies.size()] );

    final ICoreRunnableWithProgress operation = new ImportWaterBodiesOperation( waterBodies, m_data );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
    {
      new StatusDialog2( getShell(), status, getWindowTitle() );
      return false;
    }

    return true;
  }
}