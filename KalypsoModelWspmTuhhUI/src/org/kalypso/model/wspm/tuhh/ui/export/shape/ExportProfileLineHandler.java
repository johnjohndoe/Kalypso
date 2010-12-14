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
package org.kalypso.model.wspm.tuhh.ui.export.shape;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.gml.ui.commands.exportshape.ExportShapeHandler;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.export.AbstractExportProfilesHandler;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class ExportProfileLineHandler extends AbstractExportProfilesHandler
{
  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.AbstractExportProfilesHandler#createWizard(org.eclipse.core.commands.ExecutionEvent,
   *      org.kalypso.model.wspm.ui.action.ProfileSelection)
   */
  @Override
  protected IWizard createWizard( final ExecutionEvent event, final ProfileSelection profileSelection ) throws ExecutionException
  {
    final ISelection selection = HandlerUtil.getCurrentSelectionChecked( event );
    final String fileName = getFilename( profileSelection, selection );

    return new ExportProfileLineWizard( profileSelection, fileName );
  }

  private String getFilename( final ProfileSelection profileSelection, final ISelection selection )
  {
    final String fileName = ExportShapeHandler.findFileName( selection );
    if( !StringUtils.isEmpty( fileName ) )
      return fileName;

    // if no theme, we should use the container.
    final Feature container = profileSelection.getContainer();
    if( container != null )
      return FeatureHelper.getAnnotationValue( container, IAnnotation.ANNO_LABEL );

    final IProfileFeature[] profiles = profileSelection.getProfiles();
    if( !ArrayUtils.isEmpty( profiles ) )
      return profiles[0].getName();

    return null;
  }
}
