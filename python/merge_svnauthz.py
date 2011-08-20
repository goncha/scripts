#!/bin/env python
import sys
from os.path import isdir, isfile, join
from os import listdir

from ConfigParser import ConfigParser

def is_user_defined_group_ref(group_ref):
    if group_ref.startswith('@') and not group_ref.startswith('@grp__'):
        return True
    else:
        return False

def user_defined_group_ref(group_ref, trac_name):
    if is_user_defined_group_ref(group_ref):
        return '@grp__' + trac_name + '_' + group_ref[1:]
    else:
        return group_ref

def user_defined_group(group_name, trac_name):
    return 'grp__' + trac_name + '_' + group_name

def is_builtin_group(group_name):
    return group_name.startswith('grp__')

def is_builtin_group_ref(group_ref):
    return group_ref.startswith('@grp__')


def read_config(trac_env_dir):
    cp = ConfigParser()
    cp.read(join(trac_env_dir, 'conf', 'svnauthz.ini'))
    return cp


def config_to_dict(config):
    ret = {}
    for sect in config.sections():
        ret[sect] = config.items(sect)
    return ret


def dict_to_config (dictionary):
    ret = ConfigParser()
    for sect, opts in dictionary.items():
        ret.add_section(sect)
    for sect, opts in dictionary.items():
        for opt, val in opts:
           ret.set(sect, opt, val)
    return ret


def merge_svnauthz(merged_svnauthz, trac_name, svnauthz):
    users = set()
    groups = []
    if 'groups' in svnauthz:
       groups.extend(svnauthz['groups'])
       del svnauthz['groups']

    # authorization of directories
    for name, opts in svnauthz.items():
        new_opts = []
        for opt, val in opts:
           if is_user_defined_group_ref(opt):
               new_opts.append((user_defined_group_ref(opt, trac_name), val))
           else:
               new_opts.append((opt, val))
               if opt != '*' and not opt.startswith('@'):
                  users.add(opt)

        merged_svnauthz[trac_name + ':' + name] = new_opts

    # definition of groups
    new_groups = []
    for group, members in groups:
        members = [m.strip() for m in members.split(',')]
        if is_builtin_group(group):
            opt = group
        else:
            opt = user_defined_group(group, trac_name)
        val = ', '.join([user_defined_group_ref(m, trac_name)
                         for m in members])
        new_groups.append((opt, val))
        users.update([m for m in members if not m.startswith('@')])

    if len(users) > 0:
        all_members_name = 'grp__' + trac_name
        if not all_members_name in [name for name, defs in new_groups]:
            new_groups.append((all_members_name, ', '.join(users),))

    if 'groups' in merged_svnauthz:
        merged_svnauthz['groups'].extend(new_groups)
    else:
        merged_svnauthz['groups'] = new_groups


def merge_svnauthzs(svn_env_dirs):
    ret  = {}
    for trac_name, trac_dir in svn_env_dirs:
        svnauthz = config_to_dict(read_config(trac_dir))
        merge_svnauthz(ret, trac_name, svnauthz)
    return ret


def list_envs_with_svnauthz(trac_env_parent_dir):
    return [(x,join(trac_env_parent_dir, x),) \
                for x in listdir(trac_env_parent_dir) \
                if isdir(join(trac_env_parent_dir, x)) and \
                isfile(join(trac_env_parent_dir, x, 'conf', 'svnauthz.ini'))]

def help():
    print 'Usage: merge_svnauthz <trac_env_parent_dir> <merged_file>'


if __name__ == '__main__':
    if len(sys.argv) != 3:
        help()

    trac_env_parent_dir = sys.argv[1]
    merged_file = sys.argv[2]

    svn_env_dirs = list_envs_with_svnauthz(trac_env_parent_dir)
    svnauthz = merge_svnauthzs(svn_env_dirs)
    config = dict_to_config(svnauthz)

    out = file(merged_file, 'w+')
    config.write(out)
    out.flush()
    out.close()
