package org.kalypso.risk.model.schema.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class AdministrationUnit extends AbstractFeatureBinder implements IAdministrationUnit
{
  public AdministrationUnit( final Feature featureToBind )
  {
    super( featureToBind, IAdministrationUnit.QNAME );
  }

}
