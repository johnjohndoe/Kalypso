package de.tuhh.wb.javagis.data.event;

import ejb.event.EJBEvent;

public interface VersionListener
{
    public void onVersionChanged(EJBEvent event);
}
