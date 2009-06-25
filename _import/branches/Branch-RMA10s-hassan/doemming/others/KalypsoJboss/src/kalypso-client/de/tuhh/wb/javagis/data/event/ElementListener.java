package de.tuhh.wb.javagis.data.event;

public interface ElementListener
{
    public void onElementCreate(int elementTable,Object eId);
    public void onElementRemove(int elementTable,Object eId);
}
