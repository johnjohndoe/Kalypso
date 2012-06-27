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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides mechanism to create delete command
 * 
 * @author Patrice Congo
 * @author Thomas Jung
 */
public class DeleteCmdFactory
{
  /**
   * Creates a delete command for a given feature. <BR>
   * Important: The change-event method has to be called from outside. The features changed by the commands can be
   * accessed via the getter methods of the commands. Supported features are:
   * <ul>
   * <li>PolyElementFeature
   * <li>Element1DFeature
   * <li>JunctionFeature
   */
  public static final IDiscrModel1d2dChangeCommand createDeleteCmd( final Feature feature, final IFEDiscretisationModel1d2d model1d2d )
  {
    Assert.throwIAEOnNullParam( feature, "feature" ); //$NON-NLS-1$
    if( TypeInfo.isPolyElementFeature( feature ) )
    {
      return new DeletePolyElementCmd( model1d2d, feature );
    }
    else if( TypeInfo.isElement1DFeature( feature ) )
    {
      return new DeleteElement1DCmd( model1d2d, feature );
    }
    else
    {
      return null;
    }
  }

  public static final void createDeleteCmd( final IFEDiscretisationModel1d2d model1d2d, final EasyFeatureWrapper[] selected, final ChangeDiscretiationModelCommand modelChangeCmd )
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( selected, "selected" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( modelChangeCmd, "modelChangeCmd" ); //$NON-NLS-1$

    for( final EasyFeatureWrapper easyFeatureWrapper : selected )
    {
      if( easyFeatureWrapper == null )
      {
        throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory.4") ); //$NON-NLS-1$
      }
      try
      {
        final Feature feature = easyFeatureWrapper.getFeature();
        if( feature != null )
        {
          final IDiscrModel1d2dChangeCommand delCmd = createDeleteCmd( feature, model1d2d );
          if( delCmd != null )
          {
            modelChangeCmd.addCommand( delCmd );
          }
          else
          {
            throw new UnsupportedOperationException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory.5") + feature ); //$NON-NLS-1$
          }
        }
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }
    }
  }

  /**
   * Creates a delete command. <BR>
   * <ul>
   * <li>PolyElementFeature
   */
  public static final IDiscrModel1d2dChangeCommand createDeleteCmdPoly(  final IFEDiscretisationModel1d2d model1d2d )
  {
    return new DeletePolyElementCmd( model1d2d, null );
  }
  /**
   * Creates a delete command. <BR>
   * <ul>
   * <li>Element1DFeature
   */
  public static final IDiscrModel1d2dChangeCommand createDeleteCmd1dElement( final IFEDiscretisationModel1d2d model1d2d )
  {
    return new DeleteElement1DCmd( model1d2d, null );
  }
}
