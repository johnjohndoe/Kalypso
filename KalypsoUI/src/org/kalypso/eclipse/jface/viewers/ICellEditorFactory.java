package org.kalypso.eclipse.jface.viewers;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.factory.FactoryException;

/**
 * @author Belger
 */
public interface ICellEditorFactory
{
  public AbstractFeatureCellEditor createEditor( final FeatureTypeProperty ftp, final IProject project, final Composite parent, final int style ) throws FactoryException;
}
