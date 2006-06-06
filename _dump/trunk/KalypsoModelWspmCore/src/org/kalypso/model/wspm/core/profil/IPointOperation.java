package org.kalypso.model.wspm.core.profil;

import java.util.LinkedList;


public interface IPointOperation
{
public Object[] doOperation(final LinkedList<IProfilPoint> points, final Object[] values);

}
