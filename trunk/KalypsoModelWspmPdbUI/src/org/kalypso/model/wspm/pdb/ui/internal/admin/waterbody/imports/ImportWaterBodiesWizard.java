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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPart;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;

/**
 * A wizard for importing water bodies into a pdb.
 *
 * @author Gernot Belger
 */
public class ImportWaterBodiesWizard extends AbstractImportWaterBodiesWizard
{
  @Override
  protected WaterBody[] initData( final IWorkbenchPart part, final IStructuredSelection selection )
  {
    /* The session. */
    Session session = null;

    try
    {
      /* Check the viewer. */
      if( !(part instanceof IConnectionViewer) )
        throw new IllegalStateException( "Part must be of the type IConnectionViewer..." );

      /* Cast. */
      final IConnectionViewer viewer = (IConnectionViewer) part;

      /* Get the connection. */
      final IPdbConnection connection = viewer.getConnection();

      /* Open the session. */
      session = connection.openSession();

      /* Get the existing waterbodies. */
      final WaterBody[] existingWaterbodies = GetPdbList.getArray( session, WaterBody.class );

      /* Close the session. */
      session.close();

      return existingWaterbodies;
    }
    catch( final PdbConnectException ex )
    {
      // TODO
      ex.printStackTrace();

      return new WaterBody[] {};
    }
    finally
    {
      /* Close the session. */
      PdbUtils.closeSessionQuietly( session );
    }
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    /* Get the import water bodies data. */
    final ImportWaterBodiesData data = getData();

    /* Get the water bodies. */
    final WritableSet selectedWaterBodies = data.getSelectedWaterBodies();
    final WaterBody[] waterBodies = (WaterBody[]) selectedWaterBodies.toArray( new WaterBody[selectedWaterBodies.size()] );

    /* Get the workbench part. */
    final IWorkbenchPart part = getPart();
    if( (part == null) || !(part instanceof IConnectionViewer) )
      throw new IllegalStateException( "Part must be of the type IConnectionViewer..." );

    /* Get the connection. */
    final IConnectionViewer viewer = (IConnectionViewer) part;
    final IPdbConnection connection = viewer.getConnection();

    /* Create the operation. */
    final ICoreRunnableWithProgress operation = new ImportWaterBodiesOperation( waterBodies, data, connection );

    /* Execute the operation. */
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
    {
      final StatusDialog dialog = new StatusDialog( getShell(), status, getWindowTitle() );
      dialog.open();
    }

    /* Select the new element in tree. */
    final ElementSelector selector = new ElementSelector();
    if( waterBodies.length > 0 )
      selector.addWaterBodyName( waterBodies[0].getName() );

    /* Reload the data. */
    viewer.reload( selector );

    return !status.matches( IStatus.ERROR );
  }
}