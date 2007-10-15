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
package org.kalypso.model.wspm.sobek.core.wizard;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.INodeTypes.NODE_TYPE;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class FNWizardBuilder
{

  public static IWorkbenchWizard getWizard( final Feature feature )
  {
    if( ISobekConstants.QN_HYDRAULIC_SOBEK_BRANCH.equals( feature.getFeatureType().getQName() ) )
      return new FNWizardEditBranch( feature );

    final NODE_TYPE type = NODE_TYPE.getFeatureType( feature );
    switch( type )
    {
      case eCrossSectionNode:
        throw (new NotImplementedException());
// return new FNWizardEditCrossSectionNode( workspace, feature );

      default:
        throw new IllegalStateException();
    }
  }

}
