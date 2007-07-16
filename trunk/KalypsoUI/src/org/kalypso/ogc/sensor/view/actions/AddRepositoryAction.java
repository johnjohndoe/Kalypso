/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.view.actions;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.ui.ImageProvider;

/**
 * Ein Repository hinzuf�gen.
 * 
 * @author schlienger
 */
public class AddRepositoryAction extends AbstractObservationChooserAction
{
  public AddRepositoryAction( final ObservationChooser explorer )
  {
    super( explorer, "Repository hinzuf�gen", ImageProvider.IMAGE_ZML_REPOSITORY_ADD, "F�gt ein Repository hinzu..." );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  @Override
  public void run( )
  {
    final Runnable runnable = new AddRepositoryRunnable( getRepositoryContainer(), getShell() );
    runnable.run();
  }

  /**
   * @author schlienger
   */
  public static class ChooseRepositoryLabelProvider extends LabelProvider
  {
    private final Image m_image;

    public ChooseRepositoryLabelProvider( )
    {
      m_image = ImageProvider.IMAGE_ZML_REPOSITORY.createImage();
    }

    /**
     * @see org.eclipse.jface.viewers.LabelProvider#dispose()
     */
    @Override
    public void dispose( )
    {
      m_image.dispose();

      super.dispose();
    }

    /**
     * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
     */
    @Override
    public Image getImage( final Object element )
    {
      return m_image;
    }
  }
}